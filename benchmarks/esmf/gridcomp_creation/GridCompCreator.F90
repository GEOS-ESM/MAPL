#include "MAPL_Generic.h"
module grid_comp_creator

   use shared_constants
   use grid_comp_creator_memory_profiler
   use mapl_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use ESMF
   use mpi
   use, intrinsic :: iso_fortran_env, only: I64 => int64, R64 => real64

   implicit none
   private

   public :: GridCompCreator
   public :: run_creator
   public :: finalize_creator
   public :: finalize_all

   type :: GC_Container
      type(ESMF_GridComp) :: gc
   end type GC_Container

   type :: GridCompCreator
      character(len=:), allocatable :: name
      integer :: npes = -1
      integer :: ngc = -1
      integer :: rank = -1
      integer :: comm = MPI_COMM_WORLD
      type(GC_Container), allocatable :: gcc(:)
      character(len=:), allocatable :: parameter_filename
   contains
      procedure :: initialize => initialize_creator
      procedure :: finalize => finalize_creator
      procedure :: run => run_creator
   end type GridCompCreator

   interface GridCompCreator
      module procedure :: construct_creator
   end interface GridCompCreator

   character(len=*), parameter :: name = 'GridCompCreator'

contains

   function construct_creator(num_gc, param_file) result(creator)
      type(GridCompCreator) :: creator
      integer, intent(in) :: num_gc
      character(len=*), optional, intent(in) :: param_file

      creator%name = name
      creator%parameter_filename = ''
      if(present(param_file)) creator%parameter_filename = param_file 
      creator%ngc = num_gc

   end function construct_creator

   subroutine initialize_creator(this, rc)
      class(GridCompCreator), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_VM) :: vm
      integer :: npes_world

      _ASSERT(.not. allocated(this%gcc), 'gcc is already allocated.')
      allocate(this%gcc(this%ngc))
      !call initialize_mpi(this, _RC)
      call ESMF_Initialize(vm=vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=this%comm, _RC)
      call MPI_Comm_rank(this%comm, this%rank, _IERROR)
      call MPI_Comm_size(this%comm, this%npes, _IERROR)

      _RETURN(_SUCCESS)

   end subroutine initialize_creator

   subroutine initialize_mpi(creator, rc) 
      class (GridCompCreator), intent(inout) :: creator
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: ierror
      integer :: npes_world

      call MPI_Init(ierror)
      call MPI_Comm_rank(creator%comm, creator%rank, _IERROR)
      call MPI_Comm_size(creator%comm, npes_world, _IERROR)
      if (creator%npes == -1) creator%npes = npes_world
      _ASSERT(npes_world >= creator%npes, "npes_world is smaller than npes")

      _RETURN(_SUCCESS)

   end subroutine initialize_mpi

   subroutine finalize_creator(this, rc)
      class(GridCompCreator), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call destroy_containers(this%gcc, _RC)
      if(allocated(this%gcc)) deallocate(this%gcc)
      call ESMF_Finalize()
      _RETURN(_SUCCESS)

   end subroutine finalize_creator

   subroutine finalize_all(creator, rc)
      class(GridCompCreator), intent(inout) :: creator
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_Finalize()

   end subroutine finalize_all

   subroutine run_creator(this, time, mem_used, mem_commit, rc)
      class(GridCompCreator), intent(inout) :: this
      real(kind=R64), intent(out) :: time
      class(MemoryProfile), intent(out) :: mem_used
      class(MemoryProfile), intent(out) :: mem_commit
      integer, optional, intent(out) :: rc
      integer :: status

      call initialize_creator(this, _RC)
      call run_gridcomp_creation(this%gcc, time, mem_used, mem_commit, _RC)
!      call finalize_creator(this, _RC)

      _RETURN(_SUCCESS)

   end subroutine run_creator

   subroutine run_gridcomp_creation(gcc, time, mem_used, mem_commit, rc)
      type(GC_Container), intent(inout) :: gcc(:)
      real(kind=R64), intent(out) :: time
      type(MemoryProfile), intent(out) :: mem_used
      type(MemoryProfile), intent(out) :: mem_commit
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      integer(kind=I64) :: start, end_, count_rate
      type(MemoryProfile) :: mem_used_before, mem_commit_before

      call profile_memory(mem_used_before, mem_commit_before, _RC)
      call system_clock(count = start, count_rate=count_rate)
      do i = 1, size(gcc)
         gcc(i) = make_container(i, _RC)
      end do
      call system_clock(count = end_, count_rate=count_rate)
      call profile_memory(mem_used, mem_commit, _RC)
      time = real(end_ - start, R64) / count_rate
      mem_used    = mem_used - mem_used_before
      mem_commit  = mem_commit - mem_commit_before
      _RETURN(_SUCCESS)
      
   end subroutine run_gridcomp_creation

   function make_gc_name(n, rc) result(gc_name)
      character(len=MAXSTR) :: gc_name
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: FMT_ = '(I0)'
      character(len=MAXSTR) :: raw

      write(raw, fmt=FMT_, iostat=status) n
      _ASSERT(status == _SUCCESS, 'Unable to make gridcomp name')
      gc_name = "GC" // trim(raw)
      _RETURN(_SUCCESS)

   end function make_gc_name

   function make_container(n, rc) result(gcc)
      type(GC_Container) :: gcc
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status

      gcc%gc = ESMF_GridCompCreate(name=trim(make_gc_name(n)), _RC)
      _RETURN(_SUCCESS)

   end function make_container

   subroutine destroy_containers(gcc, rc)
      type(GC_Container), intent(inout) :: gcc(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      
      do i=1, size(gcc)
         call ESMF_GridCompDestroy(gcc(i)%gc, _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine destroy_containers

end module grid_comp_creator

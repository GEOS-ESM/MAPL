#include "MAPL_Generic.h"
module grid_comp_creator

   use grid_comp_creation_shared
   use grid_comp_creator_memory_profiler
   use mapl_ErrorHandlingMod
   use esmf
   use mpi
   use, intrinsic :: iso_fortran_env, only: I64 => int64

   implicit none
   private

   public :: GridCompCreator
   public :: write_results
   public :: finalize
   public :: run
   public :: assignment(=)

   type :: GC_Container
      type(ESMF_GridComp) :: gc
   end type GC_Container

   type :: GridCompCreator
      integer :: ngc = -1
      real(R64) :: time = -1.0
      integer :: rank = -1
      type(MemoryProfile), allocatable :: prior_memory
      type(MemoryProfile), allocatable :: post_memory
      character(len=:), allocatable, private :: name
      integer, private :: npes = -1
      integer, private :: comm = MPI_COMM_WORLD
   end type GridCompCreator

   interface GridCompCreator
      module procedure :: construct_creator
   end interface GridCompCreator

   interface run
      module procedure :: run_creator
   end interface run

   type :: String
      character(len=:), allocatable :: characters
   end type String

   interface String
      module procedure :: construct_string
   end interface String

   interface assignment(=)
      module procedure :: assign_characters_to_string
      module procedure :: assign_string_to_characters
   end interface assignment(=)

   character(len=*), parameter :: name = 'GridCompCreator'
   character(len=*), parameter :: BLANK = ''

   type(String), allocatable :: module_creator_results(:)

contains

   function get_creator_results() result(results)
      type(String), allocatable :: results(:)
      
      allocate(results(0))
      if(allocated(module_creator_results)) results = module_creator_results

   end function get_creator_results

   function construct_string(ch) result(s)
      type(String) :: s
      character(len=*), optional, intent(in) :: ch

      s%characters = BLANK
      if(present(ch)) s%characters = trim(ch)

   end function construct_string

   subroutine assign_characters_to_string(s, ch)
      type(String), intent(out) :: s
      character(len=*), intent(in) :: ch

      s = String(ch)

   end subroutine assign_characters_to_string

   subroutine assign_string_to_characters(ch, s)
      character(len=*), intent(out) :: ch
      type(String), intent(in) :: s

      ch = s%characters

   end subroutine assign_string_to_characters

   function construct_creator(num_gc) result(creator)
      type(GridCompCreator) :: creator
      integer, intent(in) :: num_gc

      creator%name = name
      creator%ngc = num_gc

   end function construct_creator

   subroutine initialize_creator(creator, rc)
      class(GridCompCreator), intent(inout) :: creator
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_VM) :: vm

      call ESMF_Initialize(vm=vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=creator%comm, _RC)
      call MPI_Comm_rank(creator%comm, creator%rank, _IERROR)
      call MPI_Comm_size(creator%comm, creator%npes, _IERROR)
      _RETURN(_SUCCESS)

   end subroutine initialize_creator

   subroutine finalize(rc)
      integer, optional, intent(out) :: rc

      call ESMF_Finalize()
      _RETURN(_SUCCESS)

   end subroutine finalize

   subroutine run_creator(creator, rc)
      class(GridCompCreator), intent(inout) :: creator
      integer, optional, intent(out) :: rc
      integer :: status

      call initialize_creator(creator, _RC)
      call run_gridcomp_creation(creator, _RC)
      if(creator%rank == 0) call write_creator_results(creator, module_creator_results)
      _RETURN(_SUCCESS)

   end subroutine run_creator

   subroutine run_gridcomp_creation(creator, rc)
      class(GridCompCreator), intent(inout) :: creator
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      type(GC_Container), allocatable :: gcc(:)
      integer(kind=I64) :: start, end_, count_rate

      _ASSERT(creator%ngc >= 0, 'Cannot create a negative number of gridcomps')

      if(.not. allocated(creator%prior_memory)) then
         call profile_memory(creator%prior_memory, _RC)
      end if

      call system_clock(count = start, count_rate=count_rate)

      allocate(gcc(creator%ngc))
      do i = 1, size(gcc)
         gcc(i) = make_container(i, _RC)
      end do
      call system_clock(count = end_, count_rate=count_rate)
      call profile_memory(creator%post_memory, _RC)
      creator%time = real(end_ - start, R64) / count_rate
      call destroy_containers(gcc, _RC)
      if(allocated(gcc)) deallocate(gcc)

      _RETURN(_SUCCESS)
      
   end subroutine run_gridcomp_creation

   function memory_delta(creator, rc) result(diff)
      type(MemoryProfile) :: diff
      class(GridCompCreator), intent(in) :: creator
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(creator%prior_memory), 'prior memory is not allocated.')
      _ASSERT(allocated(creator%post_memory), 'post memory is not allocated.')
      diff = creator%post_memory - creator%prior_memory
      _RETURN(_SUCCESS)

   end function memory_delta

   function make_gc_name(n, rc) result(gc_name)
      character(len=:), allocatable :: gc_name
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

      gcc%gc = ESMF_GridCompCreate(name=make_gc_name(n), _RC)
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

   subroutine write_creator_results(creator, results)
      class(GridCompCreator), intent(in) :: creator
      type(String), allocatable, intent(out) :: results(:)
      character(len=*), parameter :: NGC_TIME_HEADER = 'num_components, time(s)'
      type(MemoryProfile) :: diff
      character(len=:), allocatable :: line
      integer :: status
      integer, parameter :: INDENT_SIZE = 4
      character(len=*), parameter :: indent = repeat(' ', INDENT_SIZE)
      character(len=2), parameter :: COMMENT = '# '

      allocate(results(4))
      results(1) = COMMENT // NGC_TIME_HEADER // JOIN // MEMORY_PROFILE_HEADER
      line = to_characters(creator%ngc) // JOIN // to_characters(creator%time)
      results(2) = line // JOIN // print_memory_profile(creator%prior_memory) // ' ' // COMMENT // 'BEFORE'
      results(3) = line // JOIN // print_memory_profile(creator%post_memory) // ' ' // COMMENT // 'AFTER'
      if(creator%post_memory == creator%prior_memory) then
         results(4) = COMMENT // 'Memory profile did not change.'
         return
      end if

      diff = memory_delta(creator, rc=status)

      if(status /= 0) then
         results(4) = COMMENT // 'Unable to find delta'
         return
      end if

      results(4) = line // JOIN // print_memory_profile(diff)

   end subroutine write_creator_results

   subroutine write_results(creator)
      class(GridCompCreator), intent(in) :: creator
      integer :: i
      type(String), allocatable :: results(:)
      
      if(creator%rank /= 0) return
      results = get_creator_results()
      do i = 1, size(results)
         write(*, '(A)') results(i)%characters
      end do

   end subroutine write_results

end module grid_comp_creator

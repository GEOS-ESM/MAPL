#include "MAPL_Generic.h"
module grid_comp_creator

   use grid_comp_creation_shared
   use grid_comp_creator_memory_profiler
   use strings
   use mapl_ErrorHandlingMod
   use esmf
   use mpi
   use, intrinsic :: iso_fortran_env, only: R64 => real64, I64 => int64

   implicit none
   private

   public :: GridCompCreator
   public :: write_results
   public :: finalize
   public :: run
   public :: assignment(=)

   ! This should be changed into a parent component.
   ! Then the program calls a driver procedure that creates
   ! the parent grid component, sets services (initialize,
   ! run, and finalize). Then it calls initialize, run, and
   ! finalize. Initialize includes setting up the profiling
   ! structures as internal state.
   ! Child components are created in parent run
   ! which also includes profiling. finalize collects the
   ! information for the driver procedure and returns it
   ! to the program which outputs it.
   type :: GridCompCreator
      integer :: ngc = -1
      real(R64) :: time = -1.0
      integer :: rank = -1
      type(MemoryProfile), allocatable :: prior_memory
      type(MemoryProfile), allocatable :: post_memory
      character(len=:), allocatable, private :: name
      integer, private :: npes = -1
      integer, private :: comm = MPI_COMM_WORLD
      type(ESMF_GridComp), pointer :: gc => null()
   end type GridCompCreator

   interface GridCompCreator
      module procedure :: construct_creator
   end interface GridCompCreator

   interface run
      module procedure :: run_creator
   end interface run

   type(ESMF_LogKind_Flag), parameter :: LOG_KIND_FLAG = ESMF_LOGKIND_NONE
   character(len=*), parameter :: name = 'GridCompCreator'
   character(len=*), parameter :: BLANK = ''
   type(String), allocatable :: module_creator_results(:)
   type(ESMF_GridComp), pointer :: gc_ptr => null()

contains

   subroutine creation_driver(num_gc, rc)
      integer, intent(in) :: num_gc
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_GridComp), target :: gc

      gc = create_creator_gridcomp(num_gc, _RC)
      gc_ptr => gc
      ! need to set services
      ! need to initialize gridcomp
      ! need to run gridcomp
      ! need to finalize gridcomp
   end subroutine creation_driver

   function create_creator_gridcomp(num_gc, rc) result(gc)
      integer, intent(in) :: num_gc
      type(ESMF_GridComp) :: gc
      integer, optional, intent(out) :: rc
      integer :: status

      if(num_gc < 1) then
         _RETURN(_FAILURE)
         return
      end if

      gc = make_gridcomp(is_parent = .TRUE., _RC)

   end function create_creator_gridcomp

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

      call ESMF_Initialize(vm=vm,logKindFlag=LOG_KIND_FLAG, _RC)
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
      type(ESMF_GridComp), allocatable :: gcc(:)
      integer(kind=I64) :: start, end_, count_rate

      _ASSERT(creator%ngc >= 0, 'Cannot create a negative number of gridcomps')

      if(.not. allocated(creator%prior_memory)) then
         call profile_memory(creator%prior_memory, _RC)
      end if

      call system_clock(count = start, count_rate=count_rate)

      allocate(gcc(creator%ngc))
      do i = 1, size(gcc)
         gcc(i) = make_gridcomp(i, _RC)
      end do
      call system_clock(count = end_, count_rate=count_rate)
      call profile_memory(creator%post_memory, _RC)
      creator%time = real(end_ - start, R64) / count_rate
      call destroy_gridcomps(gcc, _RC)
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
      integer, optional, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: FMT_ = '(I0)'
      character(len=MAXSTR) :: raw

      raw = '_'
      if(present(n)) write(raw, fmt=FMT_, iostat=status) n
      _ASSERT(status == _SUCCESS, 'Unable to make gridcomp name')
      gc_name = "GC" // trim(raw)
      _RETURN(_SUCCESS)

   end function make_gc_name

   function make_gridcomp(n, is_parent, rc) result(gc)
      type(ESMF_GridComp) :: gc
      integer, intent(in) :: n
      logical, optional, intent(in) :: is_parent
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Context_Flag) :: contextflag
      character(len=:), allocatable :: name
      
      context_flag = ESMF_CONTEXT_PARENT_VM
      if(present(is_parent)) then
         if(is_parent) then
            context_flag = ESMF_CONTEXT_OWN_VM
            raw = make_gc_name()
         end if
      end if
      
      if(.not. allocated(name)) then
         name = make_gc_name(n, _RC)
      end if

      gc = ESMF_GridCompCreate(name=name, contextflag=context_flag, _RC)

      _RETURN(_SUCCESS)

   end function make_gridcomp

   subroutine destroy_gridcomps(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      
      do i=1, size(gc)
         call ESMF_GridCompDestroy(gc(i), _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine destroy_gridcomps

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

   function get_creator_results() result(results)
      type(String), allocatable :: results(:)
      
      allocate(results(0))
      if(allocated(module_creator_results)) results = module_creator_results

   end function get_creator_results

end module grid_comp_creator

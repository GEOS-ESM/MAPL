#include "MAPL_Generic.h"
module grid_comp_creator

   use grid_comp_creation_shared
   use grid_comp_creator_memory_profiler
   use mapl_ErrorHandlingMod
   use esmf
   use mpi
   use, intrinsic :: iso_fortran_env, only: R64 => real64, I64 => int64

   implicit none
   private

   public :: run
   public :: finalize

   type :: CreatorData
      ! parameters
      integer :: ngc = -1
      logical :: use_own_vm = .FALSE.
      ! result data
      integer(kind=I64) :: start_time = -1_I64
      integer(kind=I64) :: end_time = -1_I64
      integer(kind=I64) :: count_rate = -1_I64
      type(MemoryProfile) :: prior
      type(MemoryProfile) :: post
      logical :: valid = .FALSE.
   contains
      procedure :: write_results => write_creator_results
      procedure :: run_options => write_run_options
   end type CreatorData

   type :: Wrapper
      type(CreatorData), pointer :: ptr => null()
   end type Wrapper

   interface CreatorData
      module procedure :: construct_creator_data
   end interface CreatorData

   interface run
      module procedure :: creation_driver
   end interface run

contains

   function construct_creator_data(num_gc, use_own_vm) result(r)
      type(CreatorData) :: r
      integer, intent(in) :: num_gc
      logical, intent(in) :: use_own_vm

      r%valid = num_gc >= 0
      if(.not. r%valid) return
      r%ngc = num_gc
      r%use_own_vm = use_own_vm

   end function construct_creator_data

   subroutine creation_driver(num_gc, use_own_vm, rc)
      integer, intent(in) :: num_gc
      logical, intent(in) :: use_own_vm
      integer, optional, intent(out) :: rc
      integer :: status
      type(CreatorData) :: cdata
      type(ESMF_GridComp) :: gc

      cdata = CreatorData(num_gc, use_own_vm)   
      _ASSERT(cdata%valid, 'Number of gridcomponents is negative.')
      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE, _RC)
      gc = ESMF_GridCompCreate(name='GC_', contextFlag=ESMF_CONTEXT_OWN_VM, _RC)
      call ESMF_GridCompSetServices(gc, userRoutine=GC_SetServices, _RC)
      call GC_SetInternal(gc, cdata, _RC)
      call ESMF_GridCompRun(gc, _RC)
      call ESMF_GridCompFinalize(gc, _RC)
      _RETURN(_SUCCESS)

   end subroutine creation_driver

   subroutine GC_SetServices(gc, rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc
      integer :: status
      type(CreatorData), pointer :: cdata
      type(Wrapper) :: wrap

      allocate(cdata, stat=status)
      _VERIFY(status)
      wrap%ptr => cdata
      call ESMF_GridCompSetInternalState(gc, wrap, _RC)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, GC_Run, _RC)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE, GC_Final, _RC)
      _RETURN(_SUCCESS)

   end subroutine GC_SetServices 

   subroutine GC_SetInternal(gc, cdata, rc)
      type(ESMF_GridComp) :: gc
      class(CreatorData), target, intent(in) :: cdata
      integer, optional, intent(out) :: rc
      integer :: status
      type(Wrapper) :: wrap

      call ESMF_GridCompGetInternalState(gc, wrap, _RC)
      wrap%ptr => cdata
      call ESMF_GridCompSetInternalState(gc, wrap, _RC)
      _RETURN(_SUCCESS)

   end subroutine GC_SetInternal

   subroutine GC_GetInternal(gc, cdata, rc)
      type(ESMF_GridComp) :: gc
      type(CreatorData), pointer, intent(out) :: cdata
      integer, optional, intent(out) :: rc
      type(Wrapper) :: wrap
      integer :: status

      call ESMF_GridCompGetInternalState(gc, wrap, _RC)
      cdata => wrap%ptr
      _RETURN(_SUCCESS)

   end subroutine GC_GetInternal

   subroutine GC_Run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc 
      type(ESMF_State) :: importState 
      type(ESMF_State) :: exportState 
      type(ESMF_Clock) :: clock 
      integer, intent(out) :: rc 
      integer :: status
      type(CreatorData), pointer :: cdata
      integer(I64) :: start_time, end_time, count_rate
      type(MemoryProfile) :: prior, post
      integer :: rank
      type(ESMF_GridComp), allocatable :: gcc(:)
      integer :: i

      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      call GC_GetInternal(gc, cdata, _RC)

      call profile_memory(prior, _RC)
      call system_clock(count = start_time, count_rate=count_rate)

      allocate(gcc(cdata%ngc))
      do i = 1, size(gcc)
         gcc(i) = make_gridcomp(i, cdata%use_own_vm, _RC)
      end do
      call system_clock(count = end_time, count_rate=count_rate)
      call profile_memory(post, _RC)
      call destroy_gridcomps(gcc, _RC)
      if(allocated(gcc)) deallocate(gcc)

      call get_rank(gc, rank, _RC)
      if(rank == 0) then
         cdata%start_time = start_time
         cdata%end_time = end_time
         cdata%count_rate = count_rate
         cdata%prior = prior
         cdata%post = post
      end if

      _RETURN(_SUCCESS)

   end subroutine GC_Run

   subroutine GC_Final(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc 
      type(ESMF_State) :: importState 
      type(ESMF_State) :: exportState 
      type(ESMF_Clock) :: clock 
      integer, intent(out) :: rc 
      integer :: status
      type(CreatorData), pointer :: cdata
      integer :: rank

      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      call get_rank(gc, rank, _RC)
      if(rank == 0) then
         call GC_GetInternal(gc, cdata, _RC)
         call cdata%write_results()
      end if

      _RETURN(_SUCCESS)

   end subroutine GC_Final

   subroutine get_rank(gc, rank, rc)
      type(ESMF_GridComp) :: gc 
      integer, intent(out) :: rank
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_VM) :: vm
      integer :: comm

      call ESMF_GridCompGet(gc, vm=vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)
      call MPI_Comm_rank(comm, rank, _IERROR)
      _RETURN(_SUCCESS)

   end subroutine get_rank

   subroutine initialize(rc)
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE, _RC)
      _RETURN(_SUCCESS)

   end subroutine initialize

   integer function finalize() result(rc)
      call ESMF_Finalize(rc=rc)
   end function finalize

   function make_gc_name(n, rc) result(gc_name)
      character(len=:), allocatable :: gc_name
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTR) :: raw

      write(raw, fmt='(I0)', iostat=status) n
      _ASSERT(status == _SUCCESS, 'Unable to make gridcomp name')
      gc_name = "GC" // raw
      _RETURN(_SUCCESS)

   end function make_gc_name

   function make_gridcomp(n, use_own_vm, rc) result(gc)
      type(ESMF_GridComp) :: gc
      integer, intent(in) :: n
      logical, intent(in) :: use_own_vm
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: name
      type(ESMF_Context_Flag) :: contextflag = ESMF_CONTEXT_PARENT_VM

      if(use_own_vm) contextflag = ESMF_CONTEXT_OWN_VM
      name = make_gc_name(n, _RC)
      gc = ESMF_GridCompCreate(name=name, contextFlag=contextflag, _RC)

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

   subroutine write_creator_results(this)
      class(CreatorData), intent(in) :: this
      character(len=*), parameter :: NGC_TIME_HEADER = 'num_components, time(s)'
      character(len=:), allocatable :: line
      integer, parameter :: INDENT_SIZE = 4
      character(len=*), parameter :: indent = repeat(' ', INDENT_SIZE)
      character(len=2), parameter :: COMMENT = '# '
      character(len=*), parameter :: FMT = '(A)'
      real(R64) :: time
      character(len=MAXSTR) :: diffline

      associate(ngc=>this%ngc, pr=>this%prior, pst=>this%post,&
            & st=>this%start_time, et=>this%end_time, rate=>this%count_rate)
         time = real(et - st, R64)/rate
         line = to_characters(ngc) // JOIN // to_characters(time) // JOIN
         write(*, FMT) COMMENT // NGC_TIME_HEADER // JOIN // MEMORY_PROFILE_HEADER
         write(*, FMT) line // print_memory_profile(pr) // ' ' // COMMENT // 'BEFORE'
         write(*, FMT) line // print_memory_profile(pst) // ' ' // COMMENT // 'AFTER'
         
         diffline = COMMENT // 'Memory profile did not change.'
         if(.not. (pst == pr)) diffline = line // print_memory_profile(pst-pr) // ' ' // COMMENT // 'CHANGE'
         write(*, FMT) trim(diffline)

         write(*, FMT) COMMENT // trim(this%run_options())

      end associate

   end subroutine write_creator_results

   function write_run_options(this) result(message)
      character(len=MAXSTR) :: message
      class(CreatorData), intent(in) :: this

      message = 'use parent VM'
      if(this%use_own_vm) message = 'use own VM'

   end function write_run_options

end module grid_comp_creator

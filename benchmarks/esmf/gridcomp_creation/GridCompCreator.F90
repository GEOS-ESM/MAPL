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

   public :: run
   public :: finalize

   type :: CreatorData
      ! parameters
      integer (in) :: ngc = -1
      type(ESMF_Context_Flag) :: contextflag = ESMF_CONTEXT_PARENT_VM
      ! result data
      integer(kind=I64) :: start_time = -1_I64
      integer(kind=I64) :: end_time = -1_I64
      integer(kind=I64) :: count_rate = -1_I64
      type(MemoryProfile), allocatable :: prior
      type(MemoryProfile), allocatable :: post
      type(String), allocatable = results(:)
      logical :: valid = .FALSE.
   end type CreatorData

   type :: Wrapper
      type(CreatorData), pointer :: ptr = null()
   end type Wrapper

   interface CreatorData
      module procedure :: construct_creator_data
   end interface CreatorData

   interface run
      module procedure :: creation_driver
   end interface run

contains

   function construct_creator_data(num_gc, use_own_vm) results(r)
      type(CreatorData) :: r
      integer, intent(in) :: num_gc
      logical, intent(in) :: use_own_vm

      r%prior = MemoryProfile()
      r%post = MemoryProfile()
      r%results = String(0)
      r%valid = num_gc >= 0
      if(.not. r%valid) return
      r%ngc = num_gc
      if(use_own_vm) r%contextflag = ESMF_CONTEXT_OWN_VM

   end function construct_creator_data

   subroutine creation_driver(num_gc, results, use_own_vm, rc)
      integer, intent(in) :: num_gc
      character(len=:), allocatable, intent(out) :: results
      logical, optional, intent(in) :: use_own_vm
      integer, optional, intent(out) :: rc
      integer :: status
      type(CreatorData) :: internal
      logical :: use_own_vm_
      integer :: ngc = -1
      type(ESMF_GridComp) :: gc
      type(ESMF_VM) :: vm
      type(ESMF_State) :: defaultState
      type(ESMF_Clock) :: defaultClock

      use_own_vm_ = .FALSE.
      if(present(use_own_vm)) use_own_vm_ = use_own_vm
      internal = CreatorData(num_gc, use_own_vm_)   
      _ASSERT(internal%valid, 'Number of gridcomponents is negative.')

      defaultState = ESMF_StateCreate(name='defaultState', _RC)
      defaultClock = make_clock(_RC)

      call initialize(_RC)
      gc = ESMF_GridCompCreate(name='GC_', context_flag=ESMF_CONTEXT_OWN_VM, _RC)
      call ESMF_GridCompSetServices(gc, userRoutine=GC_SetServices, _RC)
      call GC_SetInternal(gc, internal, _RC)

      call ESMF_GridCompInitialize(gc, importState=defaultstate, &
         exportState=defaultstate, clock=defaultClock, rc=localrc)
      call ESMF_GridCompRun(gc, importState=defaultstate, &
         exportState=defaultstate, clock=defaultClock, rc=localrc)
      call ESMF_GridCompFinalize(gc, importState=defaultstate, &
         exportState=defaultstate, clock=defaultClock, rc=localrc)
      call get_results(gc, results)
      _RETURN(_SUCCESS)

   end subroutine creation_driver

   function make_clock(rc) result(clock)
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime

      call ESMF_TimeIntervalSet(timeStep, s=1, _RC)
      call ESMF_TimeSet(startTime, yy=2024, mm=1, dd=1, _RC)
      clock = ESMF_ClockCreate(timeStep, startTime, _RC)

   end function make_clock

   subroutine GC_SetServices(gc, rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc
      integer :: status
      type(CreatorData), pointer :: internal
      type(Wrapper) :: wrap

      allocate(internal, stat=status)
      _VERIFY(status)
      wrap%ptr => internal
      call ESMF_GridCompSetInternalState(gc, wrap, _RC)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, GC_Initialize, _RC)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, GC_Run, _RC)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE, GC_Final, _RC)
      _RETURN(_SUCCESS)

   end subroutine GC_SetServices 

   subroutine GC_SetInternal(gc, internal, rc)
      type(ESMF_GridComp) :: gc
      class(CreatorData), target, intent(out) :: internal
      integer, optional, intent(out) :: rc
      integer :: status
      type(Wrapper) :: wrap

      call ESMF_GridCompGetInternalState(gc, wrap, _RC)
      wrap%ptr => internal
      _RETURN(_SUCCESS)

   end subroutine GC_SetInternal

   subroutine GC_GetInternal(gc, internal, rc)
      type(ESMF_GridComp) :: gc
      type(CreatorData), pointer, intent(out) :: internal
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_GridCompGetInternalState(gc, wrap, _RC)
      internal => wrap%ptr
      _RETURN(_SUCCESS)

   end subroutine GC_GetInternal

   subroutine GC_Initialize(gc, importState, exportState, parentclock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: parentclock
      integer, intent(out) :: rc
      type(CreatorData), pointer :: internal
      type(Wrapper) :: wrap

      call GC_GetInternal(gc, internal, _RC)
      _RETURN(_SUCCESS)

   end subroutine GC_Initialize

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

      call GC_GetInternal(gc, cdata, _RC)

      if(.not. allocated(prior)) then
         call profile_memory(prior, _RC)
      end if

      call system_clock(count = start_time, count_rate=count_rate)

      allocate(gcc(cdata%ngc))
      do i = 1, size(gcc)
         gcc(i) = make_gridcomp(i, cdata%contextflag, _RC)
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

      call get_rank(gc, rank, _RC)
      if(rank == 0) then
         call GC_GetInternal(gc, cdata, _RC)
         call write_creator_results(cdata)
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

   subroutine initialize(vm, rc)
      type(ESMF_VM), intent(out) :: vm
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_Initialize(vm=vm,logKindFlag=ESMF_LOGKIND_NONE, _RC)
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine get_results(gc, results, rc)
      type(ESMF_GridComp) :: gc
      character(len=:), allocatable, intent(out) :: results(:)
      integer, optional, intent(out) :: rc
      integer :: rank, clen
      type(CreatorData), pointer :: cdata

      cdata => null()
      if(allocated(results)) deallocate(results)
      call get_rank(gc, rank, _RC)
      if(rank == 0) then
         call GC_GetInternal(gc, cdata, _RC)
         associate(r=>cdata%results)
            clen = len(r)
            allocate(character(len=len(r) :: results(size(r))
            do i = 1, size(results)
               results(i) = r(i)
            end do
         end associate
      end if

      _RETURN(_SUCCESS)

   end subroutine get_results

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

   function make_gridcomp(n, vm, rc) result(gc)
      type(ESMF_GridComp) :: gc
      integer, intent(in) :: n
      type(ESMF_Context_Flag), intent(in) :: contextflag
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: name
      logical :: child

      name = make_gc_name(n, _RC)
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

   subroutine write_creator_results(cdata)
      class(CreatorData), pointer, intent(in) :: cdata
      character(len=*), parameter :: NGC_TIME_HEADER = 'num_components, time(s)'
      type(MemoryProfile) :: diff
      character(len=:), allocatable :: line
      integer :: status
      integer, parameter :: INDENT_SIZE = 4
      character(len=*), parameter :: indent = repeat(' ', INDENT_SIZE)
      character(len=2), parameter :: COMMENT = '# '
      real(R64) :: time

      associate(ngc=>cdata%ngc, r=>cdata%results, pr=>cdata%prior, pst=>cdata%post,&
            & st=>cdata%start_time, et=>cdata%end_time, rate=>cdata%count_rate)
         allocate(r(4))
         time = real(et - st, R64)/rate
         r(1) = COMMENT // NGC_TIME_HEADER // JOIN // MEMORY_PROFILE_HEADER
         line = to_characters(ngc) // JOIN // to_characters(time) // JOIN
         r(2) = line // print_memory_profile(pr) // ' ' // COMMENT // 'BEFORE'
         r(3) = line // print_memory_profile(pst) // ' ' // COMMENT // 'AFTER'
         r(4) = COMMENT // 'Memory profile did not change.'
         if(pst == pr) return
         diff = pst - pr
         r(4) = line // JOIN // print_memory_profile(diff)
      end associate

   end subroutine write_creator_results

end module grid_comp_creator

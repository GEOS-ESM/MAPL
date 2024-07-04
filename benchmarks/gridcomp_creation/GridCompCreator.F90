#define _SET_IF_PRESENT(A, B) if(present(A)) A = B
#define _RC _SET_IF_PRESENT(rc, status)
module grid_comp_creator

   use ESMF
   use MAPL
   use shared_constants
   use, intrinsic :: iso_fortan_env, only: I64 => int64

   implicit none
   private

   public :: initialize
   public :: run

   type :: GC_Container
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState, exportState
   end type GC_Container

   character(len=:), allocatable :: parameter_filename
   integer :: npes_model
   integer :: ngc = 1

contains

   subroutine initialize(npes, param_filename, rc)
      integer, intent(in) :: npes
      character(len=*), optional, intent(in) :: param_filename
      integer, optional, intent(out) :: rc
      integer :: status

      npes_model = npes
      parameter_filename = ''
      if(present(param_filename)) parameter_filename = param_filename 
      ngc = 10
      call ESMF_Initialize(rc=status)
      _RC

   end subroutine initialize

   subroutine finalize(rc)
      type(GC_Container), intent(in) :: gcc(:)
      integer, optional, intent(out) :: rc
      integer :: status

      call destroy_containers(gcc, rc=status)
      call ESMF_Finalize(rc=status)
      _RC

   end subroutine finalize

   subroutine run(time, memory, rc)
      integer(kind=I64), intent(out) :: time
      integer(kind=I64), intent(out) :: memory
      integer, optional, intent(out) :: rc
      integer :: status
      type(GC_Container), allocatable :: gcc(:)

      allocate(gcc(ngc))
      call run_gridcomp_creation(gcc, time, memory, rc = status)
      call finalize(gcc, rc=status)
      _RC

   end subroutine run

   subroutine run_gridcomp_creation(gcc, time, memory, rc)
      type(GC_Container), intent(inout) :: gcc(:)
      integer(kind=I64), intent(in) :: ngc
      integer(kind=I64), intent(out) :: time
      integer(kind=I64), intent(out) :: memory
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind=I64) :: start_time

      call system_clock(count=start_time) 
      do i = 1, size(gcc)
         gcc(i) = make_container(i, rc=status)
      end do
      call system_clock(count=time)
      time = time - start_time
      memory = get_memory_use(rc)
      _RC
      
   end subroutine run_gridcomp_creation

   integer function get_memory_use(rc)
      integer, optional, intent(out) :: rc
      integer :: status

      get_memory_use = -1

   end function get_memory_use

   function make_gc_name(n) result(gc_name)
      character(len=MAXSTR) :: gc_name
      integer, intent(in) :: n
      integer :: ios
      character, parameter :: FMT_ = '(I0)'
      character(len=MAXSTR) :: raw

      write(raw, fmt=FMT_, iostat=ios) n
      gc_name = "GC" // n

   end function make_gc_name

   subroutine setservices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc
      integer :: status
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run_gc, rc=status)
      _RC

   end subroutine setservices

   function make_container(n, rc) result(gc)
      type(GC_Container) :: gcc
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_State) :: importState, exportState

      gcc%gc = ESMF_GridCompCreate(name=trim(make_gc_name(n)), rc=status)
      gcc%importState = ESMF_StateCreate(rc=status)
      gcc%exportState = ESMF_StateCreate(rc=status)
      call ESMF_GridCompSetServices(gcc%gc, setservices, rc=status)

   end function make_container

   subroutine run_gc(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState, exportState
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_ClockAdvance(clock, rc=status)
      _RC

   end subroutine run_rc

   subroutine destroy_containers(gcc, rc)
      type(GC_Container), intent(inout) :: gcc
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      
      do i=1, size(gcc)
         call ESMF_GridCompDestroy(gcc(i)%gc, rc=status)
      end do
      _RC

   end subroutine destroy_containers

end module grid_comp_creator

#define _HERE print*,__FILE__,__LINE__
#define RC_ rc=status
module grid_comp_creator

   use ESMF
   use shared_constants
   use, intrinsic :: iso_fortran_env, only: I64 => int64, R64 => real64

   implicit none
   private

   public :: initialize
   public :: run

   type :: GC_Container
      type(ESMF_GridComp) :: gc
   end type GC_Container

   character(len=:), allocatable :: parameter_filename
   integer :: npes_model
   integer :: ngc = 0

   type(GC_Container), allocatable :: gcc(:)

contains

   subroutine initialize(npes, num_gc, param_file, rc)
      integer, intent(in) :: npes
      integer, intent(in) :: num_gc
      character(len=*), optional, intent(in) :: param_file
      integer, optional, intent(out) :: rc
      integer :: status

      parameter_filename = ''
      if(present(param_file)) parameter_filename = param_file 
      npes_model = npes
      ngc = num_gc
      allocate(gcc(ngc))
      call ESMF_Initialize(RC_)

   end subroutine initialize

   subroutine finalize(rc)
      integer, optional, intent(out) :: rc
      integer :: status

      call destroy_containers(gcc, RC_)
      if(allocated(gcc)) deallocate(gcc)
      call ESMF_Finalize(RC_)

   end subroutine finalize

   subroutine run(time, memory, rc)
      real(kind=R64), intent(out) :: time
      integer(kind=I64), intent(out) :: memory
      integer, optional, intent(out) :: rc
      integer :: status

      call run_gridcomp_creation(gcc, time, memory, RC_)
      call finalize(RC_)

   end subroutine run

   subroutine run_gridcomp_creation(gcc, time, memory, rc)
      type(GC_Container), intent(inout) :: gcc(:)
      real(kind=R64), intent(out) :: time
      integer(kind=I64), intent(out) :: memory
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      integer(kind=I64) :: start, end_, count_rate

      call system_clock(count = start, count_rate=count_rate)
      do i = 1, size(gcc)
         gcc(i) = make_container(i, RC_)
      end do
      call system_clock(count = end_, count_rate=count_rate)
      time = real(end_ - start) / count_rate
      memory = get_memory_use(rc)
      
   end subroutine run_gridcomp_creation

   function timer(start_time)
      integer(I64) :: timer
      integer(I64), optional, intent(in) :: start_time

      call system_clock(count=timer)
      if(present(start_time)) timer = timer - start_time

   end function timer
      
   integer function get_memory_use(rc)
      integer, optional, intent(out) :: rc
      integer :: status

      get_memory_use = -1

   end function get_memory_use

   function make_gc_name(n) result(gc_name)
      character(len=MAXSTR) :: gc_name
      integer, intent(in) :: n
      integer :: ios
      character(len=*), parameter :: FMT_ = '(I0)'
      character(len=MAXSTR) :: raw

      write(raw, fmt=FMT_, iostat=ios) n
      gc_name = "GC" // trim(raw)

   end function make_gc_name

   function make_container(n, rc) result(gcc)
      type(GC_Container) :: gcc
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status

      gcc%gc = ESMF_GridCompCreate(name=trim(make_gc_name(n)), RC_)

   end function make_container

   subroutine destroy_containers(gcc, rc)
      type(GC_Container), intent(inout) :: gcc(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      
      do i=1, size(gcc)
         call ESMF_GridCompDestroy(gcc(i)%gc, RC_)
      end do

   end subroutine destroy_containers

end module grid_comp_creator

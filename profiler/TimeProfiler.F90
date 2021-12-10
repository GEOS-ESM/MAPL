#include "unused_dummy.H"

module mapl_TimeProfiler_private
   use mapl_BaseProfiler, only: BaseProfiler
   use mapl_BaseProfiler, only: TimeProfilerIterator => BaseProfilerIterator

   use mapl_MpiTimerGauge
   use mapl_AdvancedMeter
   use mapl_AbstractMeter
   use mapl_MeterNode
   implicit none
   private

   public :: TimeProfiler
   public :: TimeProfilerIterator
   public :: get_global_time_profiler

   type, extends(BaseProfiler) :: TimeProfiler
      private
   contains
      procedure :: make_meter
      procedure :: copy
   end type TimeProfiler

   interface TimeProfiler
      module procedure new_TimeProfiler
   end interface TimeProfiler

   type(TimeProfiler), protected, target :: global_time_profiler

contains


   function new_TimeProfiler(name, comm_world) result(prof)
      type(TimeProfiler), target :: prof
      character(*), intent(in) :: name
      integer, optional,intent(in) :: comm_world

      call prof%set_comm_world(comm_world = comm_world)
      call prof%set_node(MeterNode(name, prof%make_meter()))

   end function new_TimeProfiler


   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(TimeProfiler), intent(in) :: this
      _UNUSED_DUMMY(this)
      meter = AdvancedMeter(MpiTimerGauge())
   end function make_meter


   function get_global_time_profiler() result(time_profiler)
      type(TimeProfiler), pointer :: time_profiler

      time_profiler => global_time_profiler

   end function get_global_time_profiler


   subroutine copy(new, old)
      class(TimeProfiler), target, intent(inout) :: new
      class(BaseProfiler), target, intent(in) :: old

      call new%copy_profiler(old)

   end subroutine copy


end module mapl_TimeProfiler_Private



module mapl_TimeProfiler
   use mapl_BaseProfiler
   use mapl_TimeProfiler_private
   implicit none
   private

   public :: TimeProfiler
   public :: TimeProfilerIterator
   public :: get_global_time_profiler
   public :: initialize_global_time_profiler
   public :: finalize_global_time_profiler
   public :: start_global_time_profiler
   public :: stop_global_time_profiler

contains

   subroutine initialize_global_time_profiler(name)
      character(*), optional, intent(in) :: name

      type(TimeProfiler), pointer :: time_profiler
      character(:), allocatable :: name_

      if (present(name)) then
         name_ = name
      else
         name_ = 'top'
      end if

      time_profiler => get_global_time_profiler()
      time_profiler = TimeProfiler(name_)

   end subroutine initialize_global_time_profiler


   subroutine finalize_global_time_profiler()

      type(TimeProfiler), pointer :: time_profiler

      time_profiler => get_global_time_profiler()
      call time_profiler%finalize()

   end subroutine finalize_global_time_profiler


   subroutine start_global_time_profiler(name)
      character(*), intent(in) :: name
      
      type(TimeProfiler), pointer :: time_profiler

      time_profiler => get_global_time_profiler()
      call time_profiler%start(name)

   end subroutine start_global_time_profiler

   
   subroutine stop_global_time_profiler(name)
      character(*), intent(in) :: name

      type(TimeProfiler), pointer :: time_profiler

      time_profiler => get_global_time_profiler()
      call time_profiler%stop(name)

   end subroutine stop_global_time_profiler


end module mapl_TimeProfiler

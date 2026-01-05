#include "MAPL.h"

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

   type, extends(BaseProfiler) :: TimeProfiler
      private
   contains
      procedure :: make_meter

   end type TimeProfiler

   interface TimeProfiler
      module procedure new_TimeProfiler
   end interface TimeProfiler

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

end module mapl_TimeProfiler_Private

module mapl_TimeProfiler
   use mpi
   use mapl_BaseProfiler
   use mapl_TimeProfiler_private
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod

   implicit none
   private

   public :: TimeProfiler
   public :: TimeProfilerIterator

contains

end module mapl_TimeProfiler

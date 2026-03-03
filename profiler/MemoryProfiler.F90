#include "MAPL.h"
module MAPL_MemoryProfiler_private
   use MAPL_BaseProfiler, only: BaseProfiler
   use MAPL_BaseProfiler, only: MemoryProfilerIterator => BaseProfilerIterator

   use MAPL_MallocGauge
   use MAPL_RssMemoryGauge
   use MAPL_VmstatMemoryGauge
   use MAPL_AdvancedMeter
   use MAPL_AbstractMeter
   use MAPL_MeterNode
   implicit none
   private

   public :: MemoryProfiler
   public :: MemoryProfilerIterator

   type, extends(BaseProfiler) :: MemoryProfiler
      private
   contains
      procedure :: make_meter
   end type MemoryProfiler

   interface MemoryProfiler
      module procedure new_MemoryProfiler
   end interface MemoryProfiler

contains


   function new_MemoryProfiler(name, comm_world) result(prof)
      type(MemoryProfiler), target :: prof
      character(*), intent(in) :: name
      integer, optional, intent(in) :: comm_world

      call prof%set_comm_world(comm_world = comm_world)
      call prof%set_node(MeterNode(name, prof%make_meter()))

   end function new_MemoryProfiler

   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(MemoryProfiler), intent(in) :: this

      meter = AdvancedMeter(MallocGauge())

      _UNUSED_DUMMY(this)
   end function make_meter


end module MAPL_MemoryProfiler_private



module MAPL_MemoryProfiler
   use MAPL_BaseProfiler
   use MAPL_MemoryProfiler_private
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: MemoryProfiler
   public :: MemoryProfilerIterator

contains


end module MAPL_MemoryProfiler

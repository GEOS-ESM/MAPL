#include "MAPL.h"
module mapl_MemoryProfiler_private_mod
   use mapl_BaseProfiler_mod, only: BaseProfiler
   use mapl_BaseProfiler_mod, only: MemoryProfilerIterator => BaseProfilerIterator

   use mapl_MallocGauge_mod
   use mapl_RssMemoryGauge_mod
   use mapl_VmstatMemoryGauge_mod
   use mapl_AdvancedMeter_mod
   use mapl_AbstractMeter_mod
   use mapl_MeterNode_mod
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

      ! NAG OpenMP runtime workaround: Initialize per-DSO OpenMP state
      !
      ! NAG's optimized OpenMP runtime uses per-DSO lazy initialization. Each
      ! shared library gets its own OpenMP state block that is not populated until
      ! the first !$omp parallel region executes. When !$omp master directives in
      ! BaseProfiler.F90 run before initialization, they dereference a null pointer
      ! and crash. This no-op parallel region ensures the state is initialized.
      !$omp parallel
      !$omp end parallel

      call prof%set_comm_world(comm_world = comm_world)
      call prof%set_node(MeterNode(name, prof%make_meter()))

   end function new_MemoryProfiler

   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(MemoryProfiler), intent(in) :: this

      meter = AdvancedMeter(MallocGauge())

      _UNUSED_DUMMY(this)
   end function make_meter


end module mapl_MemoryProfiler_private_mod



module mapl_MemoryProfiler_mod
   use mapl_BaseProfiler_mod
   use mapl_MemoryProfiler_private_mod
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod
   implicit none
   private

   public :: MemoryProfiler
   public :: MemoryProfilerIterator

contains


end module mapl_MemoryProfiler_mod

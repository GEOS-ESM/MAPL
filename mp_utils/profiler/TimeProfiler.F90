#include "MAPL.h"

module mapl_TimeProfiler_private_mod
   use mapl_BaseProfiler_mod, only: BaseProfiler
   use mapl_BaseProfiler_mod, only: TimeProfilerIterator => BaseProfilerIterator

   use mapl_MpiTimerGauge_mod
   use mapl_AdvancedMeter_mod
   use mapl_AbstractMeter_mod
   use mapl_MeterNode_mod
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

   end function new_TimeProfiler

   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(TimeProfiler), intent(in) :: this
      _UNUSED_DUMMY(this)
      meter = AdvancedMeter(MpiTimerGauge())
   end function make_meter

end module mapl_TimeProfiler_private_mod

module mapl_TimeProfiler_mod
   use mpi
   use mapl_BaseProfiler_mod
   use mapl_TimeProfiler_private_mod
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod

   implicit none
   private

   public :: TimeProfiler
   public :: TimeProfilerIterator

contains

end module mapl_TimeProfiler_mod

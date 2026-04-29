#include "MAPL.h"
module MAPL_DistributedProfiler
   use MAPL_AbstractMeter
   use MAPL_AbstractGauge
   use MAPL_AbstractMeterNode
   use MAPL_MeterNode
   use MAPL_BaseProfiler
   use Mapl_DistributedMeter

   use MAPL_AdvancedMeter
   use MAPL_MpiTimerGauge
   implicit none
   private

   public :: DistributedProfiler

   type, extends(BaseProfiler) :: DistributedProfiler
      private
      class(AbstractGauge), allocatable :: gauge
      integer :: comm = -1
   contains
      procedure :: make_meter
      procedure :: reduce
   end type DistributedProfiler

   interface DistributedProfiler
      module procedure :: new_DistributedProfiler
   end interface DistributedProfiler
      

contains


   function new_DistributedProfiler(name, gauge, comm, comm_world) result(distributed_profiler)
      type(DistributedProfiler), target :: distributed_profiler
      character(*), intent(in) :: name
      class(AbstractGauge), intent(in) :: gauge
      integer, intent(in) :: comm
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

      distributed_profiler%gauge = gauge
      distributed_profiler%comm = comm
      
      call distributed_profiler%set_comm_world(comm_world = comm_world)
      call distributed_profiler%set_node(MeterNode(name, distributed_profiler%make_meter()))
!      call distributed_profiler%start()
      
   end function new_DistributedProfiler


   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(DistributedProfiler), intent(in) :: this

      meter = DistributedMeter(this%gauge)

   end function make_meter


   subroutine reduce(this)
      class(DistributedProfiler), target, intent(inout) :: this

      class(AbstractMeterNodeIterator), target, allocatable :: iter
      class(AbstractMeterNode), pointer :: root, node
      class(AbstractMeter), pointer :: m

      root => this%get_root_node()
      iter = root%begin()
      do while (iter /= root%end())
         node => iter%get()
         m => iter%get_meter()
         
         select type (m)
         class is (DistributedMeter)
            call m%reduce(this%comm, node%get_exclusive())
         class default
            print*,'error - wrong type (other)'
         end select
         
         call iter%next()
      end do
      
   end subroutine reduce

end module MAPL_DistributedProfiler

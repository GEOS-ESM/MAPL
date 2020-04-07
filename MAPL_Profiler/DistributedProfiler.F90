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
      procedure :: copy
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

      distributed_profiler%gauge = gauge
      distributed_profiler%comm = comm
      
      call distributed_profiler%set_comm_world(comm_world = comm_world)
      call distributed_profiler%set_node(MeterNode(name, distributed_profiler%make_meter()))
      call distributed_profiler%start()
      
   end function new_DistributedProfiler


   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(DistributedProfiler), intent(in) :: this

      meter = DistributedMeter(this%gauge)
!!$      meter = DistributedMeter(MpiTimerGauge())
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

   subroutine copy(new, old)
      class(DistributedProfiler), target, intent(inout) :: new
      class(BaseProfiler), target, intent(in) :: old

      call new%copy_profiler(old)
      select type (old)
      class is (DistributedProfiler)
         new%gauge = old%gauge
         new%comm = old%comm
      end select

   end subroutine copy

end module MAPL_DistributedProfiler

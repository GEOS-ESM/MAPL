module MAP_DistributedMeterNode
   implicit none
   private

   public :: DistributedMeterNode


   interface DistributedMeterNode
      module procedure new_DistributedMeterNode
   end interface DistributedMeterNode


contains


   function new_DistributedMeterNode(meter_node, comm) result(distributed_meter_node)
      class (AbstractMeterNode), intent(in) :: meter_node
      integer, intent(in) :: comm ! mpi _communicator

   end function new_DistributedMeterNode
   
end module MAP_DistributedMeterNode

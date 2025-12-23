#include "MAPL.h"
module MAPL_StubProfiler
   use MAPL_BaseProfiler, only: BaseProfiler
   use MAPL_DistributedProfiler
   use mapl_KeywordEnforcerMod
   use mapl_NullGauge
   use MAPL_AbstractMeter
   use MAPL_AdvancedMeter
   use MAPL_AbstractMeterNode
   use MAPL_MeterNode
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: StubProfiler
   
   type, extends(DistributedProfiler) :: StubProfiler
      private
   contains
      procedure :: make_meter
      procedure :: copy
      procedure :: start_name, start_self
      procedure :: stop_name, stop_self
      procedure :: reduce
      procedure :: get_root_node
      procedure :: get_num_meters
   end type StubProfiler

   type, extends(MeterNode) :: StubNode
   contains
      procedure :: get_num_nodes
   end type StubNode

   interface StubProfiler
      module procedure new_StubProfiler
   end interface StubProfiler

   type(StubNode), target, save :: STUB_NODE
contains


   function new_StubProfiler(name) result(prof)
      type(StubProfiler), target :: prof
      character(*), intent(in) :: name

      call prof%set_node(MeterNode(name, AdvancedMeter(NullGauge())))

   end function new_StubProfiler


   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(StubProfiler), intent(in) :: this

      meter = AdvancedMeter(NullGauge())

      _UNUSED_DUMMY(this)
   end function make_meter


   subroutine copy(new, old)
      class(StubProfiler), target, intent(inout) :: new
      class(BaseProfiler), target, intent(in) :: old

      _HERE
!#      call new%copy_profiler(old)

   end subroutine copy

   subroutine start_self(this, unusable, rc)
      class(StubProfiler), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
   end subroutine start_self


   subroutine start_name(this, name, rc)
      class(StubProfiler), target, intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc


      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(name)
   end subroutine start_name


   subroutine stop_self(this, rc)
      class(StubProfiler), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine stop_self


   subroutine stop_name(this, name, rc)
      class(StubProfiler), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc


      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(name)
   end subroutine stop_name
   
   subroutine reduce(this)
      class(StubProfiler), target, intent(inout) :: this

      _UNUSED_DUMMY(this)
   end subroutine reduce

   function get_root_node(this) result(root_node)
      class(AbstractMeterNode), pointer :: root_node
      class(StubProfiler), target, intent(in) :: this
      
      root_node => STUB_NODE
   end function get_root_node

   integer function get_num_meters(this) result(num_meters)
      class(StubProfiler), intent(in) :: this
      num_meters = 0
   end function get_num_meters

   integer function get_num_nodes(this) result(num_nodes)
      class(StubNode), target, intent(in) :: this
      num_nodes = 0
      _UNUSED_DUMMY(this)
   end function get_num_nodes
   
end module MAPL_StubProfiler

#include "MAPL_Generic.h"

module mapl3g_StateSpec
   use mapl3g_StateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_StateItemSpecMap
   use mapl3g_VariableSpec
   use mapl3g_VerticalGrid
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVector
   use mapl_ErrorHandling
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use ESMF
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: StateSpec
   type, extends(StateItemSpec) :: StateSpec
      private
      type(ESMF_State) :: payload
      type(StateItemSpecMap) :: item_specs
   contains
      procedure :: set_geometry
      procedure :: add_item
      procedure :: get_item

      procedure :: create
      procedure :: destroy
      procedure :: allocate
      
      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: extension_cost
      procedure :: make_adapters

      procedure :: add_to_state
      procedure :: add_to_bundle

   end type StateSpec


contains

   ! Nothing defined at this time.
   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(StateSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom)
      _UNUSED_DUMMY(vertical_grid)
   end subroutine set_geometry

   subroutine add_item(this, name, item)
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      class(StateItemSpec), intent(in) :: item

      call this%item_specs%insert(name, item)

   end subroutine add_item

   function get_item(this, name) result(item)
      class(StateItemSpec), pointer :: item
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name

      integer :: status

      item => this%item_specs%at(name, rc=status)

   end function get_item


   subroutine create(this, rc)
      class(StateSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      
      this%payload = ESMF_StateCreate(_RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine destroy(this, rc)
      class(StateSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_StateDestroy(this%payload, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! NO-OP
   subroutine allocate(this, rc)
      class(StateSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)

      _UNUSED_DUMMY(this)
   end subroutine allocate

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(StateSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc

      select type (src_spec)
      class is (StateSpec)
         this%payload = src_spec%payload
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to


   logical function can_connect_to(this, src_spec, rc)
      class(StateSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      can_connect_to = same_type_as(src_spec, this)

      _RETURN(_SUCCESS)

   end function can_connect_to


   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(StateSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _FAIL('unimplemented')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(multi_state)
      _UNUSED_DUMMY(actual_pt)
   end subroutine add_to_state


   subroutine add_to_bundle(this, bundle, rc)
      class(StateSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use item of type InvalidSpec')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(bundle)
   end subroutine add_to_bundle
   

   recursive subroutine make_extension(this, dst_spec, new_spec, action, rc)
      class(StateSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      class(StateItemSpec), allocatable, intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      action = NullAction() ! default
      new_spec = this

      _FAIL('not implemented')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(dst_spec)
   end subroutine make_extension

   integer function extension_cost(this, src_spec, rc) result(cost)
      class(StateSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      cost = 0

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(src_spec)
   end function extension_cost


   function make_adapters(this, goal_spec, rc) result(adapters)
      type(StateItemAdapterWrapper), allocatable :: adapters(:)
      class(StateSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc


      allocate(adapters(0))
      _FAIL('unimplemented')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_spec)
   end function make_adapters

end module mapl3g_StateSpec

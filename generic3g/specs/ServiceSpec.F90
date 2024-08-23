#include "MAPL_Generic.h"

module mapl3g_ServiceSpec
   use mapl3g_StateItemSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl3g_AbstractActionSpec
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl_ErrorHandling
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl3g_VerticalGrid
   use esmf
   use gftl2_StringVector
   implicit none
   private

   public :: ServiceSpec

   type, extends(StateItemSpec) :: ServiceSpec
      private
      type(ESMF_Typekind_Flag), allocatable :: typekind
      type(ESMF_FieldBundle) :: payload
      type(StateItemSpecPtr), allocatable :: dependency_specs(:)

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: extension_cost
      procedure :: add_to_state
      procedure :: add_to_bundle
      procedure :: initialize
!!$      procedure :: check_complete
   end type ServiceSpec

   interface ServiceSpec
      module procedure new_ServiceSpec
   end interface ServiceSpec

contains

   !wdb fixme deleteme Needs a constructor with VariableSpec argument or no argument
   !wdb fixme deleteme Needs an initialize method to satisfy StateItemSpec interface
   function new_ServiceSpec(service_item_specs) result(spec)
      type(ServiceSpec) :: spec
      type(StateItemSpecPtr), intent(in) :: service_item_specs(:)

      integer :: status

      spec%dependency_specs = service_item_specs

   end function new_ServiceSpec

   subroutine create(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldBundleCreate(_RC)

      _RETURN(_SUCCESS)
   end subroutine create

   subroutine allocate(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(StateItemSpec), pointer :: spec

      associate (dep_specs => this%dependency_specs)
        do i = 1, size(dep_specs)
           spec => dep_specs(i)%ptr
           call spec%add_to_bundle(this%payload, _RC)
        end do
      end associate
    
      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(ServiceSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: alias
      character(:), allocatable :: short_name
      type(ESMF_State) :: substate
      integer :: status

      short_name = actual_pt%get_esmf_name()
      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)

      ! Add bundle to both import and export specs.
      call get_substate(multi_state%importstate, actual_pt%get_comp_name(), substate=substate, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)
      call get_substate(multi_state%exportstate, actual_pt%get_comp_name(), substate=substate, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(ServiceSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('ServiceService::Cannot nest bundles.')
   end subroutine add_to_bundle

   
   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(ServiceSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused

      integer, optional, intent(out) :: rc

      integer :: fieldCount
      type(ESMF_Field), allocatable :: fieldList(:)
      integer :: status
      logical :: can_connect

      can_connect = this%can_connect_to(src_spec, _RC)
      _ASSERT(can_connect, 'illegal connection')

      select type (src_spec)
      class is (ServiceSpec)
         src_spec%dependency_specs = [src_spec%dependency_specs, this%dependency_specs]
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to

    logical function can_connect_to(this, src_spec, rc)
      class(ServiceSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc


      select type(src_spec)
      class is (ServiceSpec)
         can_connect_to = .true.
      class default
         can_connect_to = .false.
      end select

      _RETURN(_SUCCESS)
   end function can_connect_to


  subroutine destroy(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleDestroy(this%payload, noGarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   subroutine make_extension(this, dst_spec, new_spec, action, rc)
      class(ServiceSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      class(StateItemSpec), allocatable, intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      integer :: status

      action = NullAction() ! default
      new_spec = this

      _FAIL('not implemented')
   end subroutine make_extension

   integer function extension_cost(this, src_spec, rc) result(cost)
      class(ServiceSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc
      cost = 0
      _RETURN(_SUCCESS)
   end function extension_cost

   subroutine initialize(this, geom, vertical_grid, rc)
      class(ServiceSpec), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom
      class(VerticalGrid), intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)
   end subroutine initialize

end module mapl3g_ServiceSpec

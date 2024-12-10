#include "MAPL_Generic.h"

module mapl3g_ServiceSpec

   use mapl_ErrorHandling
   use mapl3g_StateRegistry
   use mapl3g_VariableSpec
   use mapl3g_StateItemSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemExtension
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl3g_AbstractActionSpec
   use mapl3g_ESMF_Utilities, only: get_substate
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
      type(StateRegistry), pointer :: registry
      type(VariableSpec) :: variable_spec
      type(ESMF_Typekind_Flag), allocatable :: typekind
      type(ESMF_FieldBundle) :: payload
      type(StateItemSpecPtr), allocatable :: dependency_specs(:)

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_adapters

      procedure :: add_to_state
      procedure :: add_to_bundle
      procedure :: set_geometry

      procedure :: write_formatted
!!$      procedure :: check_complete
   end type ServiceSpec

   interface ServiceSpec
      module procedure new_ServiceSpec
   end interface ServiceSpec

contains

   function new_ServiceSpec(variable_spec, registry) result(spec)
      type(ServiceSpec) :: spec
      type(VariableSpec), intent(in) :: variable_spec
      type(StateRegistry), pointer, intent(in) :: registry

      spec%variable_spec = variable_spec
      spec%registry => registry
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
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(bundle)
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

   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(ServiceSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc
      integer :: status

      integer :: i, n
      type(StateItemSpecPtr), allocatable :: specs(:)
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: primary

      associate (var_spec => this%variable_spec)
        n = var_spec%service_items%size()
        allocate(specs(n))

        do i = 1, n
           v_pt = VirtualConnectionPt(ESMF_STATEINTENT_INTERNAL, var_spec%service_items%of(i))
           ! Internal items are always unique and "primary" (owned by user)
           primary => this%registry%get_primary_extension(v_pt, _RC)
           specs(i)%ptr => primary%get_spec()
        end do
      end associate
      this%dependency_specs = specs

      _RETURN(_SUCCESS)
   end subroutine set_geometry

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(ServiceSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "ServiceSpec(write not implemented yet)"
   end subroutine write_formatted

   function make_adapters(this, goal_spec, rc) result(adapters)
      type(StateItemAdapterWrapper), allocatable :: adapters(:)
      class(ServiceSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      allocate(adapters(0))

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_spec)
   end function make_adapters

end module mapl3g_ServiceSpec

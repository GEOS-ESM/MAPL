#include "MAPL_Generic.h"

module mapl3g_ServiceSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl3g_AbstractActionSpec
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl_ErrorHandling
   use mapl3g_HierarchicalRegistry
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_VirtualConnectionPt
   use esmf
   use gftl2_StringVector
   implicit none
   private

   public :: ServiceSpec

   type, extends(AbstractStateItemSpec) :: ServiceSpec
      private
      type(ESMF_Typekind_Flag), allocatable :: typekind
      type(ESMF_FieldBundle) :: payload
      type(StringVector) :: item_names
      type(StateItemSpecPtr), allocatable :: dependency_specs(:)

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_dependencies

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: extension_cost
      procedure :: make_action
      procedure :: add_to_state
      procedure :: add_to_bundle
!!$      procedure :: check_complete
   end type ServiceSpec

   interface ServiceSpec
      module procedure new_ServiceSpec
   end interface ServiceSpec

contains

   function new_ServiceSpec(item_names, rc) result(spec)
      type(ServiceSpec) :: spec
      type(StringVector), optional, intent(in) :: item_names
      integer, optional, intent(out) :: rc

      integer :: status

      if (present(item_names)) then
         spec%item_names = item_names
      end if
      
      _RETURN(_SUCCESS)
   end function new_ServiceSpec

   subroutine create(this, dependency_specs, rc)
      class(ServiceSpec), intent(inout) :: this
      type(StateItemSpecPtr), intent(in) :: dependency_specs(:)
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldBundleCreate(_RC)
      this%dependency_specs = dependency_specs

      _RETURN(_SUCCESS)
   end subroutine create

   function get_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(ServiceSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ActualConnectionPt) :: a_pt

      do i = 1, this%item_names%size()
         a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='internal', short_name=this%item_names%of(i)))
         call dependencies%push_back(a_pt)
      end do

      _RETURN(_SUCCESS)
   end function get_dependencies

   subroutine allocate(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(AbstractStateItemSpec), pointer :: spec

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
      class(AbstractStateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused

      integer, optional, intent(out) :: rc

      integer :: fieldCount
      type(ESMF_Field), allocatable :: fieldList(:)
      integer :: status

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (ServiceSpec)
         src_spec%dependency_specs = [src_spec%dependency_specs, this%dependency_specs]
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to

    logical function can_connect_to(this, src_spec)
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (ServiceSpec)
         can_connect_to = .true.
      class default
         can_connect_to = .false.
      end select

   end function can_connect_to


  subroutine destroy(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleDestroy(this%payload, noGarbage=.true., _RC)
      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   function make_action(this, dst_spec, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status

      action = NullAction()

      _RETURN(_SUCCESS)
   end function make_action

   function make_extension(this, dst_spec, rc) result(extension)
      class(AbstractStateItemSpec), allocatable :: extension
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
   end function make_extension
   
   integer function extension_cost(this, src_spec, rc) result(cost)
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc
      cost = 0
      _RETURN(_SUCCESS)
   end function extension_cost
   


end module mapl3g_ServiceSpec

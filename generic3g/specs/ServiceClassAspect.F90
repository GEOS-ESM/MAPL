#include "MAPL_Generic.h"

module mapl3g_ServiceClassAspect
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_StateRegistry
   use mapl3g_StateItemSpec
   use mapl3g_Multistate
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_StateItemExtension
   use mapl3g_NullAction
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl_ErrorHandling
   use gftl2_StringVector
   use esmf
   implicit none
   private

   public :: ServiceClassAspect

   type, extends(ClassAspect) :: ServiceClassAspect
      type(ESMF_FieldBundle) :: payload

      class(StateItemSpec), allocatable :: reference_spec

      ! Associtaed with subscriber
      type(StateRegistry), pointer :: registry => null()
      type(StringVector) :: subscriber_item_names

      ! Associated with provider
      type(StateItemSpecPtr), allocatable :: items_to_service(:)
   contains
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: matches
      procedure :: make_action
      procedure :: connect_to_export

      procedure :: get_aspect_order
      procedure :: create
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: connect_to_import
   end type ServiceClassAspect

   interface ServiceClassAspect
      procedure new_ServiceClassAspect
   end interface ServiceClassAspect

contains

   function new_ServiceClassAspect(registry, subscriber_item_names) result(service_aspect)
      type(ServiceClassAspect) :: service_aspect
      type(StateRegistry), optional, target, intent(in) :: registry
      type(StringVector), optional, intent(in) :: subscriber_item_names
      
      if (present(registry)) then
         service_aspect%registry => registry
      end if

      if (present(subscriber_item_names)) then
         service_aspect%subscriber_item_names = subscriber_item_names
      end if

      allocate(service_aspect%items_to_service(0))
      
   end function new_ServiceClassAspect

   logical function supports_conversion_general(src)
      class(ServiceClassAspect), intent(in) :: src
      supports_conversion_general = .false.
      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(ServiceClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.

      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific


   subroutine create(this, rc)
      class(ServiceClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldBundleCreate(_RC)

      _RETURN(_SUCCESS)
   end subroutine create

   subroutine destroy(this, rc)
      class(ServiceClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleDestroy(this%payload, noGarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy

   subroutine allocate(this, other_aspects, rc)
      class(ServiceClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(FieldClassAspect) :: field_aspect
      class(StateItemAspect), pointer :: aspect
      class(StateItemSpec), pointer :: spec

      associate (specs => this%items_to_service)
        do i = 1, size(specs)
           spec => specs(i)%ptr
           aspect => spec%get_aspect(CLASS_ASPECT_ID, _RC)
           field_aspect = to_FieldClassAspect(aspect, _RC)
           call field_aspect%add_to_bundle(this%payload, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(ServiceClassAspect), intent(in) :: this
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


   logical function matches(src, dst)
      class(ServiceClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.
      select type(dst)
      type is (ServiceClassAspect)
         matches = .true.
      end select

   end function matches

   function make_action(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(ServiceClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      
      action = NullAction()

      _RETURN(_SUCCESS)
   end function make_action


   ! Eventually this ServiceClassAspect should be split into multiple
   ! classes.  We cheat a bit here to get only the right subset of
   ! items added to the import payload.
   subroutine connect_to_export(this, export, actual_pt, rc)
      class(ServiceClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(FieldClassAspect) :: field_aspect
      class(StateItemAspect), pointer :: aspect
      class(StateItemSpec), pointer :: spec
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: primary

      associate (items => this%subscriber_item_names)
        do i = 1, items%size()
           v_pt = VirtualConnectionPt(ESMF_STATEINTENT_INTERNAL, items%of(i))
           primary => this%registry%get_primary_extension(v_pt, _RC)
           spec => primary%get_spec()
           aspect => spec%get_aspect(CLASS_ASPECT_ID, _RC)
           field_aspect = to_FieldClassAspect(aspect, _RC)
           call field_aspect%add_to_bundle(this%payload, _RC)
        end do
      end associate
      
      _RETURN(_SUCCESS)
   end subroutine connect_to_export

   subroutine connect_to_import(this, import, rc)
      class(ServiceClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: import
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n
      type(StateItemSpecPtr), allocatable :: spec_ptrs(:)
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: primary

      select type (import)
      type is (ServiceClassAspect)
         associate (item_names => import%subscriber_item_names)
           n = item_names%size()
           allocate(spec_ptrs(n))
           do i = 1, n
              v_pt = VirtualConnectionPt(ESMF_STATEINTENT_INTERNAL, item_names%of(i))
              ! Internal items are always unique and "primary" (owned by user)
              primary => import%registry%get_primary_extension(v_pt, _RC)
              spec_ptrs(i)%ptr => primary%get_spec()
           end do
         end associate
         this%items_to_service = [this%items_to_service, spec_ptrs]
      class default
         _FAIL('Import must be a Service')
      end select

      _RETURN(_SUCCESS)
   end subroutine connect_to_import

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(ServiceClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [CLASS_ASPECT_ID]

      _RETURN(_SUCCESS)
      
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order
 
 
end module mapl3g_ServiceClassAspect

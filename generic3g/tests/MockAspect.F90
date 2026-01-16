#include "MAPL.h"

module MockAspect_mod
   use mapl3g_AspectId
   use mapl3g_VariableSpec
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemSpec
   use mapl3g_StateItemAspect
   use mapl3g_StateRegistry
   use mapl3g_StateItemSpec
   use mapl3g_StateItemAllocation
   use mapl3g_ExtensionTransform
   use mapl3g_FieldInfo, only: FieldInfoSetInternal
   use mapl3g_Field_API, only: mapl_FieldSet, mapl_FieldGet
   use mapl3g_ClassAspect
   use mapl3g_NullTransform
   use mapl3g_MultiState
   use mapl3g_VirtualConnectionPtVector
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: MockAspect
   public :: MockClassAspect
   public :: MockItemSpec

   type, extends(ClassAspect) :: MockClassAspect
      type(ESMF_Field) :: payload
   contains
      procedure :: matches => matches_class
      procedure :: make_transform => make_transform_class
      procedure :: connect_to_export => connect_to_export_class
      procedure :: supports_conversion_general => supports_conversion_general_class
      procedure :: supports_conversion_specific => supports_conversion_specific_class
      procedure :: get_aspect_order => get_aspect_order_class
      procedure :: create => create_class
      procedure :: activate => activate_class
      procedure :: allocate => allocate_class
      procedure :: destroy => destroy_class
      procedure :: add_to_state => add_to_state_class
      procedure :: add_to_bundle => add_to_bundle_class
      procedure, nopass :: get_aspect_id => get_class_aspect_id
      procedure :: get_payload => get_payload_class
   end type MockClassAspect

   type, extends(StateItemAspect) :: MockAspect
      integer :: value = -1
      logical :: supports_conversion_ = .false.
   contains
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: update_payload
      procedure :: update_from_payload
      procedure, nopass :: get_aspect_id => get_mock_aspect_id
   end type MockAspect

   interface MockClassAspect
      module procedure :: new_MockClassAspect
   end interface MockClassAspect

   interface MockAspect
      procedure :: new_MockAspect
   end interface MockAspect

contains

   function MockItemSpec(value, state_intent, short_name, typekind, units, mirror, time_dependent, supports_conversion) result(mock_spec)
      type(StateItemSpec), target :: mock_spec
      integer, intent(in) :: value
      type(ESMF_StateIntent_Flag), optional, intent(in) :: state_intent
      character(*), optional, intent(in) :: short_name
      type(ESMF_Typekind_Flag), optional, intent(in) :: typekind
      character(*), optional, intent(in) :: units
      logical, optional, intent(in) :: mirror
      logical, optional, intent(in) :: time_dependent
      logical, optional, intent(in) :: supports_conversion

      type(MockAspect) :: mock_aspect
      type(MockClassAspect) :: mock_class_aspect

      logical :: mirror_
      logical :: time_dependent_
      logical :: supports_conversion_
      type(ESMF_StateIntent_Flag) :: state_intent_
      type(VariableSpec), target :: var_spec
      character(:), allocatable :: short_name_, units_
      type(VirtualConnectionPtVector) :: dependencies
      type(AspectMap), pointer :: aspects
      type(StateRegistry), target :: registry

      mirror_ = .false.
      if (present(mirror)) mirror_ = mirror

      time_dependent_ = .false.
      if (present(time_dependent)) time_dependent_ = time_dependent

      supports_conversion_ = .false.
      if (present(supports_conversion)) supports_conversion_ = supports_conversion

      state_intent_ = ESMF_STATEINTENT_EXPORT
      if (present(state_intent)) state_intent_ = state_intent

      short_name_ = 'AAA'
      if (present(short_name)) short_name_ = short_name

      units_ = 'barn'
      if (present(units)) units_ = units

      var_spec = make_VariableSpec(state_intent=state_intent_, short_name=short_name_, typekind=typekind, units=units_)
      mock_spec = var_spec%make_StateItemSpec(registry)
      aspects => mock_spec%get_aspects()

      mock_class_aspect = MockClassAspect(typekind, units_)
      call aspects%insert(CLASS_ASPECT_ID, mock_class_aspect)

      mock_aspect = MockAspect(value, mirror_, time_dependent_, supports_conversion_)
      call aspects%insert(MOCK_ASPECT_ID, mock_aspect)

      call mock_spec%create()


   end function MockItemSpec

   function new_MockClassAspect(typekind, units) result(aspect)
      type(MockClassAspect) :: aspect
      type(ESMF_Typekind_Flag), optional, intent(in) :: typekind
      character(*), optional, intent(in) :: units

      integer :: rc

      aspect%payload = ESMF_FieldEmptyCreate(rc=rc)
      call MAPL_FieldSet(aspect%payload, typekind=typekind, units=units, rc=rc)

   end function new_MockClassAspect

   function new_MockAspect(value, mirror, time_dependent, supports_conversion) result(aspect)
      type(MockAspect) :: aspect
      integer, intent(in) :: value
      logical, intent(in) :: mirror
      logical, intent(in) :: time_dependent
      logical, intent(in) :: supports_conversion

      call aspect%set_mirror(mirror)
      call aspect%set_time_dependent(time_dependent)

      aspect%value = value
      aspect%supports_conversion_ = supports_conversion

   end function new_MockAspect

   logical function matches(src, dst)
      class(MockAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type (dst)
      type is (MockAspect)
         matches = (src%value == dst%value)
      class default
         matches = .false.
      end select
   end function matches

   logical function supports_conversion_general(src)
     class(MockAspect), intent(in) :: src
      supports_conversion_general = src%supports_conversion_
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(MockAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      supports_conversion_specific = src%supports_conversion_
   end function supports_conversion_specific

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(MockAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      allocate(transform,source=NullTransform()) ! just in case
      if (present(rc)) rc = 0

   end function make_transform

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(MockAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      select type (this)
      type is (MockAspect)
         select type (export)
         type is (MockAspect)
            this = export
         class default
            _FAIL('bad subtype')
         end select
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   subroutine update_payload(this, field, bundle, state, rc)
      class(MockAspect), intent(in) :: this
      type(ESMF_Field), optional, intent(inout) :: field
      type(ESMF_FieldBundle), optional, intent(inout) :: bundle
      type(ESMF_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc
      ! MockAspect doesn't update payload - TypekindAspect and UnitsAspect handle that
      _RETURN(_SUCCESS)
   end subroutine update_payload

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(MockAspect), intent(inout) :: this
      type(ESMF_Field), optional, intent(in) :: field
      type(ESMF_FieldBundle), optional, intent(in) :: bundle
      type(ESMF_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc
      ! MockAspect doesn't update from payload - TypekindAspect and UnitsAspect handle that
      _RETURN(_SUCCESS)
   end subroutine update_from_payload

   function get_mock_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = MOCK_ASPECT_ID
   end function get_mock_aspect_id

   ! MockClassAspect methods
   logical function matches_class(src, dst)
      class(MockClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      matches_class = .true.  ! Always matches
   end function matches_class

   logical function supports_conversion_general_class(src)
      class(MockClassAspect), intent(in) :: src
      supports_conversion_general_class = .false.
   end function supports_conversion_general_class

   logical function supports_conversion_specific_class(src, dst)
      class(MockClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      supports_conversion_specific_class = .false.
   end function supports_conversion_specific_class

   function make_transform_class(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(MockClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      allocate(transform, source=NullTransform())
      if (present(rc)) rc = 0
   end function make_transform_class

   subroutine connect_to_export_class(this, export, actual_pt, rc)
      class(MockClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc
      integer :: status
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export_class

   function get_aspect_order_class(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(MockClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc
      
      class(StateItemAspect), pointer :: aspect
      integer :: value
      
      aspect => goal_aspects%at(MOCK_ASPECT_ID)
      select type (aspect)
      type is (MockAspect)
         value = aspect%value
      end select
      
      select case (value)
      case (0)
         allocate(aspect_ids(0))
      case (1)
         aspect_ids = [TYPEKIND_ASPECT_ID]
      case (3)
         aspect_ids = [TYPEKIND_ASPECT_ID, UNITS_ASPECT_ID]
      case default
         aspect_ids = [TYPEKIND_ASPECT_ID, UNITS_ASPECT_ID]
      end select
      
   end function get_aspect_order_class

   subroutine create_class(this, other_aspects, rc)
      class(MockClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      call mapl_FieldSet(this%payload, allocation_status=STATEITEM_ALLOCATION_CREATED, _RC)

      _RETURN(_SUCCESS)
   end subroutine create_class

   subroutine activate_class(this, rc)
      class(MockClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call mapl_FieldSet(this%payload, allocation_status=STATEITEM_ALLOCATION_ACTIVE, _RC)

      _RETURN(_SUCCESS)
   end subroutine activate_class

   ! Tile / Grid   X  or X, Y
   subroutine allocate_class(this, other_aspects, rc)
      class(MockClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
   end subroutine allocate_class

   subroutine destroy_class(this, rc)
      class(MockClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine destroy_class

   subroutine add_to_state_class(this, multi_state, actual_pt, rc)
      class(MockClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_State) :: state
      type(ESMF_Info) :: info
      integer :: status

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)
      call ESMF_InfoGetFromHost(state, info, _RC)
      call ESMF_InfoSet(info, key=actual_pt%get_full_name(), value=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state_class

   subroutine add_to_bundle_class(this, field_bundle, rc)
      class(MockClassAspect), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: field_bundle
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleAdd(field_bundle, [this%payload], multiflag=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle_class

   function get_class_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_class_aspect_id

   subroutine get_payload_class(this, unusable, field, bundle, state, rc)
      class(MockClassAspect), intent(in) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(esmf_Field), optional, allocatable, intent(out) :: field
      type(esmf_FieldBundle), optional, allocatable, intent(out) :: bundle
      type(esmf_State), optional, allocatable, intent(out) :: state
      integer, optional, intent(out) :: rc

      field = this%payload

      _RETURN(_SUCCESS)
   end subroutine get_payload_class


end module MockAspect_mod

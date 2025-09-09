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
   use mapl3g_ExtensionTransform
   use mapl3g_ClassAspect
   use mapl3g_NullTransform
   use mapl3g_MultiState
   use mapl3g_UnitsAspect
   use mapl3g_VirtualConnectionPtVector
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: MockAspect
   public :: MockItemSpec

   type, extends(ClassAspect) :: MockAspect
      integer :: value = -1
      logical :: supports_conversion_ = .false.

      character(:), allocatable :: internal_units
   contains
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific

      procedure :: create
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: add_to_bundle
      procedure :: get_aspect_order
      
      procedure, nopass :: get_aspect_id

      procedure :: update_units_aspect
      procedure :: update_units_info

   end type MockAspect

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

      mock_aspect = MockAspect(value, mirror_, time_dependent_, supports_conversion_)
      call aspects%insert(CLASS_ASPECT_ID, mock_aspect)

   end function MockItemSpec

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

   subroutine create(this, other_aspects, handle, rc)
      class(MockAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(in) :: handle(:)
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(handle)
   end subroutine create

   subroutine activate(this, rc)
      class(MockAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine activate

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, other_aspects, rc)
      class(MockAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc


      _RETURN(_SUCCESS)
   end subroutine allocate


   subroutine destroy(this, rc)
      class(MockAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine destroy


   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(MockAspect), intent(in) :: this
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
   end subroutine add_to_state

   subroutine add_to_bundle(this, field_bundle, rc)
      class(MockAspect), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: field_bundle
      integer, optional, intent(out) :: rc

      _FAIL('not supported')
   end subroutine add_to_bundle

 
   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(MockAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      select case (this%value)
      case (0)
         allocate(aspect_ids(0))
      case (1)
         aspect_ids = [TYPEKIND_ASPECT_ID]
      case (3)
         aspect_ids = [TYPEKIND_ASPECT_ID, UNITS_ASPECT_ID]
      case default
         aspect_ids = [TYPEKIND_ASPECT_ID, UNITS_ASPECT_ID]
      end select

   end function get_aspect_order

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

   subroutine update_units_aspect(this, units_aspect, rc)
      class(MockAspect), intent(inout) :: this
      type(UnitsAspect), intent(inout) :: units_aspect
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: units

      units = '<MIRROR>'
      if (allocated(this%internal_units)) then
         units = this%internal_units
      end if

      if (units == '<MIRROR>') then
         call units_aspect%set_mirror(.true.)
      else
         call units_aspect%set_units(units, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine update_units_aspect

   subroutine update_units_info(this, units_aspect, rc)
      class(MockAspect), intent(inout) :: this
      type(UnitsAspect), intent(inout) :: units_aspect
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: units

      if (units_aspect%is_mirror()) then
         units = '<MIRROR>'
      else
         units = units_aspect%get_units(_RC)
      end if

      this%internal_units = units

      _RETURN(_SUCCESS)
   end subroutine update_units_info


end module MockAspect_mod

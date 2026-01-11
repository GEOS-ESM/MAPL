#include "MAPL.h"

module mapl3g_StateClassAspect

   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_WildcardClassAspect
   use mapl3g_NullTransform
   use mapl3g_ExtensionTransform
   use mapl3g_MultiState
   use mapl3g_ESMF_Utilities, only: get_substate
   ! use mapl3g_State_API, only: MAPL_StateCreate, MAPL_StateInfoSetInternal
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none(type,external)
   private

   public :: StateClassAspect

   type, extends(ClassAspect) :: StateClassAspect
      private
      logical :: is_created = .false.
      type(ESMF_State) :: payload
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
   contains
      procedure :: get_aspect_order
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: matches => matches_a
      ! procedure :: connect_to_import
      procedure :: connect_to_export

      procedure :: create
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: get_payload
      procedure, nopass :: get_aspect_id
   end type StateClassAspect

   interface StateClassAspect
      procedure :: new_StateClassAspect
   end interface StateClassAspect

contains

   function new_StateClassAspect(state_intent, standard_name, long_name) result(aspect)
      type(StateClassAspect) :: aspect
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name

      aspect%state_intent = state_intent
      aspect%standard_name = "unknown"
      if (present(standard_name)) then
         aspect%standard_name = standard_name
      end if
      aspect%long_name = "unknown"
      if (present(long_name)) then
         aspect%long_name = long_name
      end if
   end function new_StateClassAspect

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(StateClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [ &
           CLASS_ASPECT_ID, &
           ATTRIBUTES_ASPECT_ID, &
           UNGRIDDED_DIMS_ASPECT_ID, &
           GEOM_ASPECT_ID, &
           VERTICAL_GRID_ASPECT_ID, &
           UNITS_ASPECT_ID, &
           TYPEKIND_ASPECT_ID &
           ]

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order

   subroutine create(this, other_aspects, rc)
      class(StateClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_StateCreate(stateIntent=this%state_intent, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine activate(this, rc)
      class(StateClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(this%payload, info, _RC)
      ! call MAPL_StateInfoSetInternal(info, is_active=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine activate

   subroutine allocate(this, other_aspects, rc)
      class(StateClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(other_aspects)
      _UNUSED_DUMMY(rc)
   end subroutine allocate

   subroutine destroy(this, rc)
      class(StateClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_StateDestroy(this%payload, noGarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy

   ! subroutine connect_to_import(this, import, rc)
   !    class(FieldClassAspect), intent(inout) :: this
   !    class(StateItemAspect), intent(in) :: import
   !    integer, optional, intent(out) :: rc

   !    type(FieldClassAspect) :: import_
   !    integer :: status

   !    _RETURN_IF(allocated(this%default_value))

   !    import_ = to_FieldClassAspect(import, _RC)
   !    if (allocated(import_%default_value)) then ! import wins (for now)
   !       this%default_value = import_%default_value
   !    end if

   !    _RETURN(_SUCCESS)
   ! end subroutine connect_to_import

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(StateClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(StateClassAspect) :: export_
      integer :: status

      export_ = to_StateClassAspect(export, _RC)
      call this%destroy(_RC) ! import is replaced by export/extension
      this%payload = export_%payload

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(export)
      _UNUSED_DUMMY(actual_pt)
      _UNUSED_DUMMY(rc)
   end subroutine connect_to_export

   function to_StateClassAspect(aspect, rc) result(state_aspect)
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc
      type(StateClassAspect) :: state_aspect ! result

      select type(aspect)
      class is (StateClassAspect)
         state_aspect = aspect
      class default
         _FAIL('aspect is not StateClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_StateClassAspect

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(StateClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      transform = NullTransform()

      _RETURN(_SUCCESS)
   end function make_transform

   logical function supports_conversion_general(src)
      class(StateClassAspect), intent(in) :: src
      supports_conversion_general = .false.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(StateClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.

      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(StateClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_State) :: alias, existing_state
      type(esmf_StateItem_Flag) :: itemType
      logical :: is_alias
      type(ESMF_State) :: state, substate
      character(:), allocatable :: full_name, inner_name, intent
      integer :: idx, status

      intent = actual_pt%get_state_intent()
      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, "/", back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      alias = ESMF_NamedAlias(this%payload, name=inner_name, _RC)
      call ESMF_StateGet(substate, itemName=inner_name, itemType=itemType, _RC)
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
         if (intent /= "import") then
            call ESMF_StateGet(substate, itemName=inner_name, nestedState=existing_state, _RC)
            is_alias = associated(alias%statep, existing_state%statep)
            _ASSERT(is_alias, 'Different states added under the same name in state.')
         end if
      end if
      call ESMF_StateAddReplace(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine get_payload(this, unusable, field, bundle, state, rc)
      class(StateClassAspect), intent(in) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(esmf_Field), optional, allocatable, intent(out) :: field
      type(esmf_FieldBundle), optional, allocatable, intent(out) :: bundle
      type(esmf_State), optional, allocatable, intent(out) :: state
      integer, optional, intent(out) :: rc

      state = this%payload

      _RETURN(_SUCCESS)

   end subroutine get_payload

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

   function matches_a(src, dst) result(matches)
      logical :: matches
      class(StateClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.
      select type(dst)
      class is (StateClassAspect)
         matches = .true.
      class is (WildcardClassAspect)
         matches = .true.
      end select

      _UNUSED_DUMMY(src)
   end function matches_a

end module mapl3g_StateClassAspect

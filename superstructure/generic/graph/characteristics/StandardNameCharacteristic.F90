#include "MAPL.h"

!------------------------------------------------------------------------------
! StandardNameCharacteristic: the "standard name" StateItemCharacteristic, a
! ValueCharacteristic holding a plain standard_name string. Names are
! compared by string equality only; whether a pair is compared at all
! (UNCHECKED on either side matches) is decided by StateItemDescriptor, not
! here. An UNCHECKED instance holds no value (get_standard_name returns '');
! its Info value is UNKNOWN_STANDARD_NAME.
!
! Two different names follow the validation mode (ValidationPolicy): in
! strict mode the pair needs an extension and build_transform reports it
! incompatible (no Transform can rename); otherwise it matches with a
! warning, and the export's name wins because the import aliases the
! export's field. An UNCHECKED export meeting a declared import also warns,
! in either mode. Both warnings are dropped when standard_name warnings are
! suppressed.
!------------------------------------------------------------------------------
module mapl_StandardNameCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ValueCharacteristic
   use mapl_StateItemCharacteristic_mod, only: report_build_transform_incompatible
   use mapl_ValidationPolicy_mod, only: validation_is_strict, standard_name_warnings_suppressed
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, STANDARD_NAME_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_UNCHECKED, CHARACTERISTIC_STATUS_SPECIFIED, operator(==)
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleSet
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, operator(==)
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StandardNameCharacteristic
   public :: UNKNOWN_STANDARD_NAME

   ! Info value for UNCHECKED standard names; also the declared value that
   ! means UNCHECKED.
   character(*), parameter :: UNKNOWN_STANDARD_NAME = '<unknown>'

   type, extends(ValueCharacteristic) :: StandardNameCharacteristic
      private
      character(:), allocatable :: standard_name
   contains
      procedure, nopass :: get_kind => standard_name_get_kind
      procedure :: needs_extension_for => standard_name_needs_extension_for
      procedure :: build_transform => standard_name_build_transform
      procedure :: get_signature => standard_name_get_signature
      procedure :: write_info => standard_name_write_info
      procedure :: match_warning => standard_name_match_warning
      procedure :: get_standard_name
   end type StandardNameCharacteristic

   interface StandardNameCharacteristic
      module procedure new_StandardNameCharacteristic
   end interface StandardNameCharacteristic

contains

   function new_StandardNameCharacteristic(standard_name) result(characteristic)
      character(*), intent(in) :: standard_name
      type(StandardNameCharacteristic) :: characteristic

      characteristic%standard_name = standard_name
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_StandardNameCharacteristic

   function standard_name_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = STANDARD_NAME_CHARACTERISTIC_KIND
   end function standard_name_get_kind

   ! `goal` is guaranteed a StandardNameCharacteristic: the caller looks both
   ! sides up by the same kind key. StateItemDescriptor calls this only when
   ! both sides are SPECIFIED. Outside strict mode a disagreement is tolerated.
   logical function standard_name_needs_extension_for(this, goal) result(needs_extension)
      class(StandardNameCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (StandardNameCharacteristic)
         needs_extension = (this%get_standard_name() /= goal%get_standard_name()) .and. validation_is_strict()
      class default
         error stop 'StandardNameCharacteristic: needs_extension_for called with a non-StandardNameCharacteristic goal - should be checked by calling procedure'
      end select
   end function standard_name_needs_extension_for

   function get_standard_name(this) result(standard_name)
      class(StandardNameCharacteristic), intent(in) :: this
      character(:), allocatable :: standard_name

      standard_name = ''
      if (allocated(this%standard_name)) standard_name = this%standard_name
   end function get_standard_name

   subroutine standard_name_build_transform(this, goal, transformer, rc)
      class(StandardNameCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_incompatible(rc)
   end subroutine standard_name_build_transform

   function standard_name_match_warning(this, goal) result(msg)
      class(StandardNameCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      character(:), allocatable :: msg

      msg = ''
      if (standard_name_warnings_suppressed()) return
      if (.not. (goal%get_status() == CHARACTERISTIC_STATUS_SPECIFIED)) return

      select type (goal)
      class is (StandardNameCharacteristic)
         if (this%get_status() == CHARACTERISTIC_STATUS_UNCHECKED) then
            msg = 'standard_name convention: Import declares standard_name "' // &
                 goal%get_standard_name() // '" but the connected Export declares none.'
         else if (this%get_status() == CHARACTERISTIC_STATUS_SPECIFIED) then
            if (this%get_standard_name() /= goal%get_standard_name()) then
               msg = 'standard_name convention violation (permissive mode): Export declares ' // &
                    'standard_name "' // this%get_standard_name() // '" but connected Import declares "' // &
                    goal%get_standard_name() // '". Connecting anyway; the Export value wins.'
            end if
         end if
      end select
   end function standard_name_match_warning

   ! Descriptive: a standard_name write does not touch the field's structure,
   ! so it is valid in either phase. UNCHECKED writes UNKNOWN_STANDARD_NAME.
   subroutine standard_name_write_info(this, handle, phase, rc)
      class(StandardNameCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: standard_name
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      _UNUSED_DUMMY(phase)

      standard_name = this%get_standard_name()
      if (this%get_status() == CHARACTERISTIC_STATUS_UNCHECKED) standard_name = UNKNOWN_STANDARD_NAME

      item_type = handle%itemType(_RC)
      if (item_type == ESMF_STATEITEM_FIELD) then
         field = handle%get_field(_RC)
         call MAPL_FieldSet(field, standard_name=standard_name, _RC)
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = handle%get_field_bundle(_RC)
         call MAPL_FieldBundleSet(bundle, standard_name=standard_name, _RC)
      else
         _FAIL('StandardNameCharacteristic: write_info needs a Field or FieldBundle handle')
      end if

      _RETURN(_SUCCESS)
   end subroutine standard_name_write_info

   function standard_name_get_signature(this) result(signature)
      class(StandardNameCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'standard_name=' // this%get_standard_name()
   end function standard_name_get_signature

end module mapl_StandardNameCharacteristic_mod

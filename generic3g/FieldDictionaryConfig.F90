#include "MAPL.h"

! FieldDictionaryConfig holds the runtime configuration for the field
! dictionary subsystem, parsed from the mapl/field_dictionary section of
! cap.yaml.  It also provides the is_exempt() predicate that determines
! which item types skip dictionary validation.

module mapl3g_FieldDictionaryConfig

   use esmf
   use mapl_ErrorHandling
   use mapl3g_ValidationMode
   use mapl3g_StateItem

   implicit none(type, external)
   private

   public :: FieldDictionaryConfig

   type :: FieldDictionaryConfig
      private
      character(:), allocatable :: dictionary_path
      type(ValidationMode) :: validation_mode
      logical :: warn_on_unverified = .true.
      logical :: warn_on_non_cf     = .false.
   contains
      procedure :: get_dictionary_path
      procedure :: get_validation_mode
      procedure :: get_warn_on_unverified
      procedure :: get_warn_on_non_cf
      procedure :: is_exempt
   end type FieldDictionaryConfig

   interface FieldDictionaryConfig
      module procedure new_default
      module procedure new_from_hconfig
   end interface FieldDictionaryConfig

contains

   ! Construct with sensible defaults (permissive, CWD dictionary)
   function new_default() result(config)
      type(FieldDictionaryConfig) :: config

      config%dictionary_path   = 'field_dictionary.yaml'
      config%validation_mode   = VALIDATION_MODE_PERMISSIVE
      config%warn_on_unverified = .true.
      config%warn_on_non_cf    = .false.
   end function new_default

   ! Construct from the mapl/field_dictionary YAML mapping node
   function new_from_hconfig(node, rc) result(config)
      type(FieldDictionaryConfig) :: config
      type(ESMF_HConfig), intent(in) :: node
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: temp_string

      ! Start from defaults
      config = FieldDictionaryConfig()

      if (ESMF_HConfigIsDefined(node, keyString='path')) then
         temp_string = ESMF_HConfigAsString(node, keyString='path', _RC)
         config%dictionary_path = temp_string
      end if

      if (ESMF_HConfigIsDefined(node, keyString='validation_mode')) then
         temp_string = ESMF_HConfigAsString(node, keyString='validation_mode', _RC)
         config%validation_mode = ValidationMode(temp_string)
      end if

      if (ESMF_HConfigIsDefined(node, keyString='warn_on_unverified')) then
         config%warn_on_unverified = ESMF_HConfigAsLogical(node, keyString='warn_on_unverified', _RC)
      end if

      if (ESMF_HConfigIsDefined(node, keyString='warn_on_non_cf')) then
         config%warn_on_non_cf = ESMF_HConfigAsLogical(node, keyString='warn_on_non_cf', _RC)
      end if

      _RETURN(_SUCCESS)
   end function new_from_hconfig

   ! Returns .true. for item types that are exempt from dictionary validation.
   ! Exempt types: SERVICE, SERVICE_PROVIDER, SERVICE_SUBSCRIBER,
   !               FIELDBUNDLE, STATE, WILDCARD, EXPRESSION
   ! Required types: FIELD, VECTOR, BRACKET, VECTORBRACKET
   logical function is_exempt(this, item_type)
      class(FieldDictionaryConfig), intent(in) :: this
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      is_exempt = &
           item_type == MAPL_STATEITEM_SERVICE          .or. &
           item_type == MAPL_STATEITEM_SERVICE_PROVIDER  .or. &
           item_type == MAPL_STATEITEM_SERVICE_SUBSCRIBER .or. &
           item_type == MAPL_STATEITEM_FIELDBUNDLE       .or. &
           item_type == MAPL_STATEITEM_STATE             .or. &
           item_type == MAPL_STATEITEM_WILDCARD          .or. &
           item_type == MAPL_STATEITEM_EXPRESSION
   end function is_exempt

   ! Accessors

   pure function get_dictionary_path(this) result(path)
      character(:), allocatable :: path
      class(FieldDictionaryConfig), intent(in) :: this
      path = this%dictionary_path
   end function get_dictionary_path

   pure function get_validation_mode(this) result(mode)
      type(ValidationMode) :: mode
      class(FieldDictionaryConfig), intent(in) :: this
      mode = this%validation_mode
   end function get_validation_mode

   pure logical function get_warn_on_unverified(this)
      class(FieldDictionaryConfig), intent(in) :: this
      get_warn_on_unverified = this%warn_on_unverified
   end function get_warn_on_unverified

   pure logical function get_warn_on_non_cf(this)
      class(FieldDictionaryConfig), intent(in) :: this
      get_warn_on_non_cf = this%warn_on_non_cf
   end function get_warn_on_non_cf

end module mapl3g_FieldDictionaryConfig

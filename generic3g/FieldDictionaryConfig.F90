#include "MAPL.h"

! FieldDictionaryConfig holds the runtime configuration for the field
! dictionary subsystem, parsed from the mapl/field_dictionary section of
! cap.yaml.  It also provides the is_exempt() predicate that determines
! which item types skip dictionary validation.
!
! The default dictionary path is 'field_dictionary.yaml' (CWD).  Experiment
! setup is expected to install/link the MAPL-installed geos_field_dictionary.yaml
! as 'field_dictionary.yaml' in the run directory.  Use has_dictionary_path()
! to distinguish a user-supplied explicit path from the default.

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
   contains
      procedure :: get_dictionary_path
      procedure :: get_validation_mode
      procedure :: has_dictionary_path
      procedure :: is_exempt
   end type FieldDictionaryConfig

   interface FieldDictionaryConfig
      module procedure new_default
      module procedure new_from_hconfig
   end interface FieldDictionaryConfig

contains

   ! Construct with sensible defaults: permissive mode, look for
   ! 'field_dictionary.yaml' in the current working directory.
   ! Experiment setup is expected to copy or link the MAPL-installed
   ! dictionary (geos_field_dictionary.yaml → field_dictionary.yaml)
   ! into the run directory before execution.
   function new_default() result(config)
      type(FieldDictionaryConfig) :: config

      config%dictionary_path   = 'field_dictionary.yaml'
      config%validation_mode   = VALIDATION_MODE_PERMISSIVE
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

      _RETURN(_SUCCESS)
   end function new_from_hconfig

   ! Returns .true. for item types that are exempt from dictionary validation.
   ! Exempt types: SERVICE, SERVICE_PROVIDER, SERVICE_SUBSCRIBER,
   !               FIELDBUNDLE, STATE, WILDCARD, EXPRESSION
   ! Required types: FIELD, VECTOR, BRACKET, VECTORBRACKET
   logical function is_exempt(this, item_type)
      class(FieldDictionaryConfig), intent(in) :: this
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      is_exempt = any(item_type == [ &
           MAPL_STATEITEM_SERVICE,            &
           MAPL_STATEITEM_SERVICE_PROVIDER,   &
           MAPL_STATEITEM_SERVICE_SUBSCRIBER, &
           MAPL_STATEITEM_FIELDBUNDLE,        &
           MAPL_STATEITEM_STATE,              &
           MAPL_STATEITEM_WILDCARD,           &
           MAPL_STATEITEM_EXPRESSION          &
           ])
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

   pure logical function has_dictionary_path(this)
      class(FieldDictionaryConfig), intent(in) :: this
      has_dictionary_path = len(this%dictionary_path) > 0
   end function has_dictionary_path

end module mapl3g_FieldDictionaryConfig

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

module mapl_FieldDictionaryConfig_mod

   use esmf
   use mapl_ErrorHandling_mod
   use mapl_ValidationMode_mod, only: MAPL_ValidationMode => ValidationMode
   use mapl_ValidationMode_mod, only: MAPL_VALIDATION_MODE_PERMISSIVE => VALIDATION_MODE_PERMISSIVE
   use mapl_StateItem_mod

   implicit none(type, external)
   private

   public :: FieldDictionaryConfig
   public :: set_field_dictionary_config
   public :: get_field_dictionary_config

   type :: FieldDictionaryConfig
      private
      character(:), allocatable :: dictionary_path
      type(MAPL_ValidationMode) :: validation_mode
   contains
      procedure :: get_dictionary_path
      procedure :: get_validation_mode
      procedure :: has_dictionary_path
      procedure :: is_exempt
   end type FieldDictionaryConfig

   interface FieldDictionaryConfig
      module procedure new_default
      module procedure new_from_hconfig
      module procedure new_from_path
   end interface FieldDictionaryConfig

   ! Module-level singleton, mirroring the_field_dictionary/get_field_dictionary
   ! in mapl_FieldDictionary_mod: set once (typically by
   ! MaplFramework%initialize_field_dictionary) and consulted thereafter by
   ! StandardNameAspect and VariableSpec's dictionary-defaulting logic so
   ! neither has to thread a FieldDictionaryConfig through every call site.
   ! Lazily defaulted (via get_field_dictionary_config) to FieldDictionaryConfig()
   ! (permissive) if never explicitly set, so code paths that run before/without
   ! cap.yaml parsing (e.g. unit tests that build aspects directly) still get a
   ! well-defined mode. (Cannot give this a non-trivial default initializer:
   ! FieldDictionaryConfig is also this module's generic constructor name, which
   ! shadows the intrinsic structure constructor for keyword-based initialization.)
   type(FieldDictionaryConfig), private, target, save :: the_field_dictionary_config
   logical, private, save :: is_config_set = .false.

contains

   ! Explicitly set the singleton (typically once, from
   ! MaplFramework%initialize_field_dictionary).
   subroutine set_field_dictionary_config(config)
      type(FieldDictionaryConfig), intent(in) :: config
      the_field_dictionary_config = config
      is_config_set = .true.
   end subroutine set_field_dictionary_config

   ! Retrieve the singleton, lazily defaulting to FieldDictionaryConfig()
   ! (permissive, default dictionary path) if set_field_dictionary_config was
   ! never called.
   function get_field_dictionary_config() result(ptr)
      type(FieldDictionaryConfig), pointer :: ptr
      if (.not. is_config_set) then
         the_field_dictionary_config = FieldDictionaryConfig()
         is_config_set = .true.
      end if
      ptr => the_field_dictionary_config
   end function get_field_dictionary_config

   ! Construct with sensible defaults: permissive mode, look for
   ! 'field_dictionary.yaml' in the current working directory.
   ! Experiment setup is expected to copy or link the MAPL-installed
   ! dictionary (geos_field_dictionary.yaml → field_dictionary.yaml)
   ! into the run directory before execution.
   function new_default() result(config)
      type(FieldDictionaryConfig) :: config

      config%dictionary_path   = 'field_dictionary.yaml'
      config%validation_mode   = MAPL_VALIDATION_MODE_PERMISSIVE
   end function new_default

   ! Construct from a bare path string - the pre-#5413 form of the
   ! `field_dictionary:` cap.yaml key (a scalar, not a mapping). Mode
   ! defaults to permissive (same as new_default), only the path differs.
   function new_from_path(path) result(config)
      type(FieldDictionaryConfig) :: config
      character(*), intent(in) :: path

      config = FieldDictionaryConfig()
      config%dictionary_path = path
   end function new_from_path

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
         config%validation_mode = MAPL_ValidationMode(temp_string)
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
      type(MAPL_ValidationMode) :: mode
      class(FieldDictionaryConfig), intent(in) :: this
      mode = this%validation_mode
   end function get_validation_mode

   pure logical function has_dictionary_path(this)
      class(FieldDictionaryConfig), intent(in) :: this
      has_dictionary_path = len(this%dictionary_path) > 0
   end function has_dictionary_path

end module mapl_FieldDictionaryConfig_mod

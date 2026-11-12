!------------------------------------------------------------------------------
! ValidationPolicy: the graph's only view of the validation settings. They
! live in the field-dictionary configuration (`field_dictionary:` YAML keys
! `validation_mode`, `suppress_standard_name_warnings`,
! `require_standard_name`), which legacy reads too, so both paths follow one
! switch; `require_quantity_type` has no legacy counterpart (quantity type is
! graph-only). Graph code asks here rather than reading
! FieldDictionaryConfig, so moving the keys touches only this module.
!
! Strict mode makes a legal pair with no Transform yet (UNSUPPORTED) an error
! at freeze and a standard_name disagreement INCOMPATIBLE; permissive mode
! (the default) warns.
!------------------------------------------------------------------------------
module mapl_ValidationPolicy_mod
   use mapl_FieldDictionaryConfig_mod, only: FieldDictionaryConfig, get_field_dictionary_config
   use mapl_ValidationMode_mod, only: VALIDATION_MODE_STRICT, operator(==)
   implicit none(type, external)
   private

   public :: validation_is_strict
   public :: standard_name_warnings_suppressed
   public :: standard_name_required
   public :: quantity_type_required

contains

   logical function validation_is_strict() result(strict)
      type(FieldDictionaryConfig), pointer :: config

      config => get_field_dictionary_config()
      strict = config%get_validation_mode() == VALIDATION_MODE_STRICT
   end function validation_is_strict

   logical function standard_name_warnings_suppressed() result(suppressed)
      type(FieldDictionaryConfig), pointer :: config

      config => get_field_dictionary_config()
      suppressed = config%suppress_standard_name_warnings_flag()
   end function standard_name_warnings_suppressed

   logical function standard_name_required() result(required)
      type(FieldDictionaryConfig), pointer :: config

      config => get_field_dictionary_config()
      required = config%require_standard_name_flag()
   end function standard_name_required

   logical function quantity_type_required() result(required)
      type(FieldDictionaryConfig), pointer :: config

      config => get_field_dictionary_config()
      required = config%require_quantity_type_flag()
   end function quantity_type_required

end module mapl_ValidationPolicy_mod

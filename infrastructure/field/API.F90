! Export umbrella for the MAPL.field library.

module mapl_field_api

   use mapl_FieldCreate_mod, only: MAPL_FieldCreate => FieldCreate
   use mapl_FieldCreate_mod, only: MAPL_FieldEmptyComplete => FieldEmptyComplete
   use mapl_FieldCreate_mod, only: MAPL_FieldsAreAliased => FieldsAreAliased&
   use mapl_FieldGet_mod, only: MAPL_FieldGet => FieldGet
   use mapl_FieldSet_mod, only: MAPL_FieldSet => FieldSet
   use mapl_FieldFill_mod, only: MAPL_FieldFill => FieldFill
   use mapl_RestartModes_mod, only: MAPL_RESTART_REQUIRED => RESTART_REQUIRED
   use mapl_RestartModes_mod, only: MAPL_RESTART_SKIP => RESTART_SKIP

   implicit none
   private

   public :: MAPL_FieldCreate
   public :: MAPL_FieldEmptyComplete
   public :: MAPL_FieldsAreAliased
   public :: MAPL_FieldGet
   public :: MAPL_FieldSet
   public :: MAPL_FieldFill
   public :: MAPL_RESTART_REQUIRED
   public :: MAPL_RESTART_SKIP

end module mapl_field_api


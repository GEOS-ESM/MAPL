! Export umbrella for the MAPL.field library.
module mapl_field_api

   use mapl_FieldCreateImpl_mod, only: MAPL_FieldCreate
   use mapl_FieldCreateImpl_mod, only: MAPL_FieldEmptyComplete
   use mapl_FieldCreateImpl_mod, only: MAPL_FieldsAreAliased
   use mapl_FieldGetImpl_mod, only: MAPL_FieldGet => FieldGet
   use mapl_FieldSetImpl_mod, only: MAPL_FieldSet => FieldSet
   use mapl_FieldFillImpl_mod, only: MAPL_FieldFill => FieldFill
   use mapl_RestartModes_mod, only: MAPL_RESTART_REQUIRED, MAPL_RESTART_SKIP

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

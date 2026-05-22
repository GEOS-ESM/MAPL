module mapl_Field_API_mod
   use mapl_FieldGetImpl_mod, only: MAPL_FieldGet => FieldGet
   use mapl_FieldSetImpl_mod, only: MAPL_FieldSet => FieldSet
   use mapl_FieldFillImpl_mod, only: MAPL_FieldFill => FieldFill
   use mapl_FieldCreateImpl_mod
   use mapl_StateItemAllocation_mod
   use mapl_RestartModes_mod
   use mapl_FieldPointerUtilities_mod, only: MAPL_AssignFptr => assign_fptr
   use mapl_FieldPointerUtilities_mod, only: MAPL_FieldClone => FieldClone
   ! Internal info should not be exposed to users
   use mapl_FieldInfo_mod

   public :: MAPL_FieldClone

end module mapl_Field_API_mod

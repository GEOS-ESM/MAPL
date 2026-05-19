module mapl_Field_API
   use mapl_FieldGet, only: MAPL_FieldGet => FieldGet
   use mapl_FieldSet, only: MAPL_FieldSet => FieldSet
   use mapl_FieldFill, only: MAPL_FieldFill => FieldFill
   use mapl_FieldCreate
   use mapl_StateItemAllocation
   use mapl_RestartModes
   use mapl_FieldPointerUtilities, only: MAPL_AssignFptr => assign_fptr
   use mapl_FieldPointerUtilities, only: MAPL_FieldClone => FieldClone
   ! Internal info should not be exposed to users
   use mapl_FieldInfo

   public :: MAPL_FieldClone

end module mapl_Field_API

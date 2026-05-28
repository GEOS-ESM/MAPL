module mapl_Field_API
   use mapl_FieldGetImpl_mod, only: MAPL_FieldGet => FieldGet
   use mapl_FieldSetImpl_mod, only: MAPL_FieldSet => FieldSet
   use mapl_FieldFillImpl_mod, only: MAPL_FieldFill => FieldFill
   use mapl_FieldCreateImpl_mod
   use mapl_Enums_internal, only: &
        StateItemAllocation                => MAPL_StateItemAllocation, &
        STATEITEM_ALLOCATION_INVALID       => MAPL_STATEITEM_ALLOCATION_INVALID, &
        STATEITEM_ALLOCATION_CREATED       => MAPL_STATEITEM_ALLOCATION_CREATED, &
        STATEITEM_ALLOCATION_INACTIVE      => MAPL_STATEITEM_ALLOCATION_INACTIVE, &
        STATEITEM_ALLOCATION_ACTIVE        => MAPL_STATEITEM_ALLOCATION_ACTIVE, &
        STATEITEM_ALLOCATION_CONNECTED     => MAPL_STATEITEM_ALLOCATION_CONNECTED, &
        STATEITEM_ALLOCATION_ALLOCATED     => MAPL_STATEITEM_ALLOCATION_ALLOCATED, &
        operator(==), operator(/=), operator(<), operator(>=)
   use mapl_RestartModes_mod
   use mapl_FieldPointerUtilities_mod, only: MAPL_AssignFptr => assign_fptr
   use mapl_FieldPointerUtilities_mod, only: MAPL_FieldClone => FieldClone
   ! Internal info should not be exposed to users
   use mapl_FieldInfo_mod

   public :: MAPL_FieldClone

end module mapl_Field_API

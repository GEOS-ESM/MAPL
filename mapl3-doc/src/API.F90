module mapl3g_Field_API
   use mapl3g_FieldGet, only: MAPL_FieldGet => FieldGet
   use mapl3g_FieldSet, only: MAPL_FieldSet => FieldSet
   use mapl3g_FieldCreate
   use mapl3g_VerticalStaggerLoc
   ! Internal info should not be exposed to users
!#   use mapl3g_FieldInfo, only: MAPL_FieldInfoGetPrivate
!#   use mapl3g_FieldInfo, only: MAPL_FieldInfoSetPrivate
!#   use mapl3g_FieldInfo, only: MAPL_FieldInfoSetShared
!#   use mapl3g_FieldInfo, only: MAPL_FieldInfoGetShared

end module mapl3g_Field_API

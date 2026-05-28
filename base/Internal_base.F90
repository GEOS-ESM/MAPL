! Internal umbrella for the MAPL base layer.
! Covers only the leaf modules compiled into MAPL.base.
! Modules from other layers (mp_utils, comms, etc.) that appear in
! API.F90 are NOT included here — they belong to their own umbrellas.
module mapl_base_internal

   use mapl_FileIO_mod
   use mapl_FileIOShared_mod
   use mapl_FileMetadataUtils_mod
   use mapl_FileMetadataUtilsVector_mod
   use mapl_NCIO_mod
   use mapl_LocStreamMod_impl_mod
   use mapl_MemUtils_mod
   use mapl_Sun_mod
   use mapl_SimpleBundleMod_impl_mod

   implicit none

end module mapl_base_internal

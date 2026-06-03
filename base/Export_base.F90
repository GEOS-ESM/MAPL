! Export umbrella for the MAPL base layer.
! Public API of base/ leaf modules. Cross-layer symbols (comms, mp_utils, etc.)
! are exposed via mapl_base_mod (API.F90) which remains the top-level umbrella.
module mapl_base_api

   use mapl_base_internal

   implicit none
   private

   public :: mapl_VarRead
   public :: FileMetadataUtils

   ! FileIO
   ! (WRITE_PARALLEL exported via mapl_base_mod to avoid conflict)

   ! FileIOShared

   ! FileMetadata
   ! (mapl_FileMetadataUtils_mod and mapl_FileMetadataUtilsVector_mod are
   !  used without only: in API.F90 — all their public symbols flow through)

   ! NCIO

   ! MemUtils

   ! Sun / SunOrbit

   ! SimpleBundle

end module mapl_base_api

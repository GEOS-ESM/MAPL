module mapl_EsmfUtils_API_mod
   use mapl_UngriddedDim_mod, only: UngriddedDim, make_UngriddedDim => make_ungriddedDim
   use mapl_UngriddedDims_mod, only: UngriddedDims
   ! ESMF Field Pointer Utilities - no public entities needed (all unused #4999)
   ! ESMF Time Utilities - exported directly via MAPL.F90 (used by statistics gridcomp)
   ! SimpleAlarm - exported directly via MAPL.F90 (used by statistics gridcomp)
   ! ISO8601 DateTime - exported directly via MAPL.F90 (used by orbit gridcomp)

   implicit none(type,external)
   private

   public :: UngriddedDim, make_UngriddedDim
   public :: UngriddedDims

end module mapl_EsmfUtils_API_mod

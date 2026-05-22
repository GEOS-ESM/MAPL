module mapl_EsmfUtils_API_mod
   use mapl_UngriddedDim_mod, only: UngriddedDim, make_UngriddedDim => make_ungriddedDim
   use mapl_UngriddedDims_mod, only: UngriddedDims

   implicit none(type,external)
   private

   public :: UngriddedDim, make_UngriddedDim
   public :: UngriddedDims

end module mapl_EsmfUtils_API_mod

module mapl3g_VM_API
   use mapl3g_vm
end module mapl3g_VM_API

module mapl3g_EsmfUtils_API
   use mapl3g_UngriddedDim, only: UngriddedDim, make_UngriddedDim => make_ungriddedDim
   use mapl3g_UngriddedDims, only: UngriddedDims

   implicit none(type,external)
   private

   public :: UngriddedDim, make_UngriddedDim
   public :: UngriddedDims

end module mapl3g_EsmfUtils_API

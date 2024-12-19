module mapl3_GenericGrid
   use ESMF, only: ESMF_Grid
   use ESMF, only: ESMF_Locstream
   implicit none
   private

   public :: GenericGrid

   type :: GenericGrid
      type(ESMF_Grid) :: grid
      type(ESMF_LocStream) :: locstream
   end type GenericGrid

end module mapl3_GenericGrid

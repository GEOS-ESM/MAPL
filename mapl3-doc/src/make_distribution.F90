#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) make_distribution_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   
contains

   module function make_distribution(im, nx) result(distribution)
      integer, allocatable :: distribution(:)
      integer, intent(in) :: im, nx

      allocate(distribution(nx))
      call MAPL_DecomposeDim(im, distribution, nx, min_DE_extent=2)

   end function make_distribution

end submodule make_distribution_smod

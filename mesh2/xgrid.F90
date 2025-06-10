#define I_AM_MAIN
#include "MAPL_ErrLog.h"

program main
   use mapl_ErrorHandling
   use esmf
   implicit none

   type(ESMF_Mesh) :: surf_types
   type(ESMF_Grid) :: atm_grid
   type(ESMF_Xgrid) :: surf_xgrid
   integer :: status

   call ESMF_Initialize(_RC)
   _HERE
   atm_grid = ESMF_GridCreateCubedSphere(tilesize=1440, &
        staggerLocList=[ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER],_RC)
   _HERE
   surf_types = ESMF_MeshCreate('surface_types.nc', fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
!#   surf_types = ESMF_MeshCreate('surface_types2.nc', fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
   _HERE
   surf_xgrid = ESMF_XgridCreate(sideAGrid=[atm_grid], sideBMesh=[surf_types], _RC)
   _HERE
   call ESMF_Finalize(_RC)
end program main


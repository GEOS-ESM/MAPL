#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) create_basic_grid_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none (type, external)


contains

   module function create_basic_grid(spec, unusable, rc) result(grid)
      type(ESMF_Grid) :: grid
      type(LatLonGeomSpec), intent(in) :: spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LatLonDecomposition) :: decomp

      lon_axis = spec%get_lon_axis()
      lat_axis = spec%get_lat_axis()
      decomp = spec%get_decomposition()

      if (lon_axis%is_periodic()) then
         grid = ESMF_GridCreate1PeriDim( &
              & countsPerDEDim1=decomp%get_lon_distribution(), &
              & countsPerDEDim2=decomp%get_lat_distribution(), &
              & indexFlag=ESMF_INDEX_DELOCAL, &
              & gridEdgeLWidth=[0,0], &
              & gridEdgeUWidth=[0,1], &
              & coordDep1=[1,2], &
              & coordDep2=[1,2], &
              & coordSys=ESMF_COORDSYS_SPH_RAD, &
              & _RC)
       else
         grid = ESMF_GridCreateNoPeriDim( &
              & countsPerDEDim1=decomp%get_lon_distribution(), &
              & countsPerDEDim2=decomp%get_lat_distribution(), &
              & indexFlag=ESMF_INDEX_DELOCAL, &
              & gridEdgeLWidth=[0,0], &
              & gridEdgeUWidth=[1,1], &
              & coordDep1=[1,2], &
              & coordDep2=[1,2], &
              & coordSys=ESMF_COORDSYS_SPH_RAD, &
              & _RC)
      end if

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, _RC)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_basic_grid

end submodule create_basic_grid_smod

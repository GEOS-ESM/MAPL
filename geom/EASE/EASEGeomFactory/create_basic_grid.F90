#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) create_basic_grid_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl3g_EASEDecomposition
   use mapl_ErrorHandlingMod
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none (type, external)

contains

   ! Build a periodic-in-longitude ESMF_Grid for an EASE cylindrical grid.
   ! EASE grids:
   !   - Are periodic in longitude ('DE' dateline: dateline on cell edge)
   !   - Have no poles ('XY': poles outside the grid domain)
   !   - Need 2D coordinate arrays (coordDep1/2 = [1,2])
   module function create_basic_grid(spec, unusable, name, rc) result(grid)
      type(ESMF_Grid) :: grid
      type(EASEGeomSpec), intent(in) :: spec
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: ims(:), jms(:)
      type(EASEDecomposition) :: decomposition
      type(ESMF_PoleKind_Flag) :: polekindflag(2)

      ! No poles
      polekindflag = ESMF_POLEKIND_NONE

      ! Use the decomposition stored in the spec.
      decomposition = spec%get_decomposition()
      ims = decomposition%get_lon_distribution()
      jms = decomposition%get_lat_distribution()

      grid = ESMF_GridCreate1PeriDim( &
           & name            = name,              &
           & countsPerDEDim1 = ims,               &
           & countsPerDEDim2 = jms,               &
           & indexFlag       = ESMF_INDEX_DELOCAL, &
           & gridEdgeLWidth  = [0, 0],             &
           & gridEdgeUWidth  = [0, 1],             &
           & coordDep1       = [1, 2],             &
           & coordDep2       = [1, 2],             &
           & coordSys        = ESMF_COORDSYS_SPH_DEG, &
           & polekindflag    = polekindflag,       &
           & _RC)

      ! Allocate coordinate storage at center and corner stagger locations
      call ESMF_GridAddCoord(grid, _RC)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_basic_grid

end submodule create_basic_grid_smod

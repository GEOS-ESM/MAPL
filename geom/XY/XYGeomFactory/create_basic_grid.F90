#include "MAPL.h"

submodule (mapl3g_XYGeomFactory) create_basic_grid_smod
   use mapl_ErrorHandlingMod
   use mapl_InternalConstants
   use esmf
   implicit none (type, external)

contains

   module function create_basic_grid(spec, unusable, rc) result(grid)
      type(ESMF_Grid) :: grid
      type(XYGeomSpec), intent(in) :: spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: infoh

      grid = ESMF_GridCreateNoPeriDim( &
           countsPerDEDim1=spec%get_ims(), &
           countsPerDEDim2=spec%get_jms(), &
           indexFlag=ESMF_INDEX_DELOCAL, &
           gridEdgeLWidth=[0,0], &
           gridEdgeUWidth=[0,1], &
           coordDep1=[1,2], &
           coordDep2=[1,2], &
           coordSys=ESMF_COORDSYS_SPH_RAD, _RC)

      ! Allocate centre coordinates
      call ESMF_GridAddCoord(grid, _RC)

      ! Optionally allocate corner coordinates
      if (spec%get_has_corners()) then
         call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)
      end if

      ! Tag the grid with type metadata for restart identification
      call ESMF_InfoGetFromHost(grid, infoh, _RC)
      if (spec%get_lm() /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_InfoSet(infoh, 'GRID_LM', spec%get_lm(), _RC)
      end if
      call ESMF_InfoSet(infoh, 'GridType', 'XY', _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_basic_grid

end submodule create_basic_grid_smod

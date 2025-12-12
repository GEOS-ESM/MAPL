#include "MAPL.h"

module mapl3g_GeomGet
   use mapl3g_GridGet
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   implicit none (type,external)
   private

   public :: GeomGet

   interface GeomGet
      procedure geom_get
   end interface GeomGet

contains



   subroutine geom_get(geom, unusable, &
        geomType, grid, locStream, mesh, xgrid, &
        dimCount, coordDimCount, &
        rc)

      type(ESMF_Geom), intent(in) :: geom
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GeomType_Flag), optional, intent(out) :: geomType
      type(esmf_Grid), optional, intent(out) :: grid
      type(esmf_LocStream), optional, intent(out) :: locStream
      type(esmf_Mesh), optional, intent(out) :: mesh
      type(esmf_XGrid), optional, intent(out) :: xgrid
      integer, optional, intent(out) :: dimCount
      integer, optional, allocatable, intent(out) :: coordDimCount(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_GeomType_Flag) :: geomType_
      type(esmf_Grid) :: grid_

      call get_subgeom(geom, geomType=geomType_, grid=grid, locStream=locStream, mesh=mesh, xgrid=xgrid, _RC)

      if (present(geomType)) then
         geomType = geomType_
      end if

      if (geomType_ == ESMF_GEOMTYPE_GRID) then
         call esmf_GeomGet(geom, grid=grid_, _RC)
         call GridGet(grid_, dimCount=dimCount, coordDimCount=coordDimCount, _RC)
      else
         _FAIL('unimplemented')
      end if

      _RETURN(_SUCCESS)

   contains

      subroutine get_subgeom(geom, unusable, geomType, grid, locStream, mesh, xgrid, rc)
         type(esmf_Geom), intent(in) :: geom
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(esmf_GeomType_Flag), optional, intent(out) :: geomType
         type(esmf_Grid), optional, intent(out) :: grid
         type(esmf_LocStream), optional, intent(out) :: locStream
         type(esmf_Mesh), optional, intent(out) :: mesh
         type(esmf_XGrid), optional, intent(out) :: xgrid
         integer, optional, intent(out) :: rc

         integer :: status

         if (present(geomType)) then
            call esmf_GeomGet(geom, geomType=geomType, _RC)
         end if

         if (present(grid)) then
            call esmf_GeomGet(geom, grid=grid, _RC)
         end if
         if (present(locStream)) then
            call esmf_GeomGet(geom, locStream=locStream, _RC)
         end if
         if (present(mesh)) then
            call esmf_GeomGet(geom, mesh=mesh, _RC)
         end if
         if (present(xgrid)) then
            call esmf_GeomGet(geom, xgrid=xgrid, _RC)
         end if

      end subroutine get_subgeom

      
   end subroutine geom_get
      
end module mapl3g_GeomGet

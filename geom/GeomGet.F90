#include "MAPL.h"

module mapl3g_GeomGet

   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none (type,external)
   private

   public :: GeomGet

   interface GeomGet
      procedure geom_get
   end interface GeomGet

contains

   subroutine geom_get(geom, unusable, &
        name, grid, &
        rc)
      type(ESMF_Geom), intent(in) :: geom
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=:), optional, allocatable, intent(out) :: name
      type(ESMF_Grid), optional, intent(out) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid_
      character(len=ESMF_MAXSTR) :: name_
      type(ESMF_GeomType_Flag) :: geomtype

      ! For now, assert that this is a Grid-based geom (future-proofing for LocStream, Mesh, XGrid)
      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
      _ASSERT(geomtype == ESMF_GEOMTYPE_GRID, 'GeomGet currently only supports Grid-based geoms')

      if (present(grid)) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
      end if

      if (present(name)) then
         call ESMF_GeomGet(geom, grid=grid_, _RC)
         call ESMF_GridGet(grid_, name=name_, _RC)
         name = trim(name_)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine geom_get

end module mapl3g_GeomGet

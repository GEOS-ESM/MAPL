#include "MAPL.h"

! ESMF frees a bundle's own geom wrapper when the bundle empties; return the
! factory geom (same id and grid), else a fresh wrapper.
module mapl_UnownedGeom_mod
   use mapl_geom_api, only: mapl_GeomGetId, mapl_GeomId, mapl_GeomManager, &
        mapl_get_geom_manager, mapl_MaplGeom
   use esmf, only: ESMF_Geom, ESMF_GeomGet, ESMF_GeomCreate, ESMF_Grid, &
        ESMF_Mesh, ESMF_GeomType_Flag, ESMF_GEOMTYPE_GRID, &
        ESMF_GEOMTYPE_MESH, ESMF_GridMatch, ESMF_GridMatch_Flag, &
        ESMF_GRIDMATCH_EXACT, operator(==), operator(>=)
   use mapl_ErrorHandling_mod
   use pflogger, only: logging, Logger
   implicit none(type, external)
   private

   public :: get_unowned_geom

contains

   subroutine get_unowned_geom(geom, unowned, rc)
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_Geom), intent(out) :: unowned
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      type(ESMF_Mesh) :: mesh
      logical :: found
      class(Logger), pointer :: lgr

      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
      if (geomtype == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
         call find_factory_geom(geom, grid, unowned, found, _RC)
         _RETURN_IF(found)
         ! No factory geom to fall back to: the returned wrapper is not
         ! stable, so a later MAPL_FieldBundleSet(geom=) using it will
         ! still force a reallocation of the bundle's members.
         lgr => logging%get_logger('mapl.generic')
         call lgr%warning('get_unowned_geom: no factory geom found for this ' // &
              'grid; returning a fresh geom wrapper with no address stability guarantee')
         unowned = ESMF_GeomCreate(grid=grid, _RC)
      else if (geomtype == ESMF_GEOMTYPE_MESH) then
         call ESMF_GeomGet(geom, mesh=mesh, _RC)
         unowned = ESMF_GeomCreate(mesh=mesh, _RC)
      else
         _FAIL('get_unowned_geom: unsupported geomtype')
      end if

      _RETURN(_SUCCESS)
   end subroutine get_unowned_geom

   ! The grid is compared as well as the id: ids minted outside the
   ! GeomManager can coincide with one of its ids.
   subroutine find_factory_geom(geom, grid, factory_geom, found, rc)
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Geom), intent(inout) :: factory_geom
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      type(mapl_GeomId) :: id
      logical :: has_id
      type(mapl_GeomManager), pointer :: geom_mgr
      type(mapl_MaplGeom), pointer :: mapl_geom
      type(ESMF_Geom) :: candidate
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: candidate_grid
      type(ESMF_GridMatch_Flag) :: match

      found = .false.
      id = mapl_GeomGetId(geom, isPresent=has_id, _RC)
      _RETURN_UNLESS(has_id)
      geom_mgr => mapl_get_geom_manager()
      _RETURN_UNLESS(geom_mgr%has_mapl_geom(id))

      mapl_geom => geom_mgr%get_mapl_geom(id, _RC)
      candidate = mapl_geom%get_geom()
      call ESMF_GeomGet(candidate, geomtype=geomtype, _RC)
      _RETURN_UNLESS(geomtype == ESMF_GEOMTYPE_GRID)
      call ESMF_GeomGet(candidate, grid=candidate_grid, _RC)
      match = ESMF_GridMatch(grid, candidate_grid, _RC)
      found = match >= ESMF_GRIDMATCH_EXACT
      if (found) factory_geom = candidate

      _RETURN(_SUCCESS)
   end subroutine find_factory_geom

end module mapl_UnownedGeom_mod

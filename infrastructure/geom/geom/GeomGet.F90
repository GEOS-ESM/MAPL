#include "MAPL.h"

! mapl_GeomAccessors: consolidated geom/grid query routines.
!
! Absorbs the former single-symbol modules mapl_GeomGet, mapl_GeomGetHorzIJIndex,
! and mapl_GridGetHorzIJIndex (deprecated stub). Consolidation was required
! because single-public-symbol modules whose name matches their sole public
! symbol trigger Intel Fortran error #6450 when renamed via USE aliases.
!
module mapl_GeomAccessors

   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl_MaplGeom, only: MaplGeom
   use mapl_GeomSpec, only: GeomSpec
   use mapl_GeomManager, only: get_mapl_geom
   use esmf

   implicit none (type,external)
   private

   public :: GeomGet
   public :: GeomGetHorzIJIndex
   public :: GridGetHorzIJIndex   ! deprecated stub — always fails

   interface GeomGet
      procedure geom_get
   end interface GeomGet

   interface GeomGetHorzIJIndex
      module procedure get_horz_ij_index_r4
      module procedure get_horz_ij_index_r8
   end interface GeomGetHorzIJIndex

   interface GridGetHorzIJIndex
      module procedure grid_get_horz_ij_index_stub
   end interface GridGetHorzIJIndex

contains

   ! ---------------------------------------------------------------------------
   ! GeomGet
   ! ---------------------------------------------------------------------------

   subroutine geom_get(geom, unusable, name, grid, rc)
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

   ! ---------------------------------------------------------------------------
   ! GeomGetHorzIJIndex
   ! ---------------------------------------------------------------------------

   subroutine get_horz_ij_index_r4(geom, lon, lat, ii, jj, local_indices, rc)
      type(ESMF_Geom), intent(in) :: geom
      real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      logical, optional, intent(in) :: local_indices
      integer, optional, intent(out) :: rc

      type(MaplGeom), pointer :: mapl_geom
      class(GeomSpec), allocatable :: spec
      integer :: status

      mapl_geom => get_mapl_geom(geom, _RC)
      spec = mapl_geom%get_spec()

      if (present(local_indices)) then
         if (local_indices) then
            call spec%get_horz_ij_index(lon=lon, lat=lat, ii=ii, jj=jj, geom=geom, _RC)
         end if
      else
         call spec%get_horz_ij_index(lon=lon, lat=lat, ii=ii, jj=jj, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r4

   subroutine get_horz_ij_index_r8(geom, lon, lat, ii, jj, local_indices, rc)
      type(ESMF_Geom), intent(in) :: geom
      real(kind=ESMF_KIND_R8), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      logical, optional, intent(in) :: local_indices
      integer, optional, intent(out) :: rc

      type(MaplGeom), pointer :: mapl_geom
      class(GeomSpec), allocatable :: spec
      integer :: status

      mapl_geom => get_mapl_geom(geom, _RC)
      spec = mapl_geom%get_spec()

      if (present(local_indices)) then
         if (local_indices) then
            call spec%get_horz_ij_index(lon=lon, lat=lat, ii=ii, jj=jj, geom=geom, _RC)
         end if
      else
         call spec%get_horz_ij_index(lon=lon, lat=lat, ii=ii, jj=jj, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r8

   ! ---------------------------------------------------------------------------
   ! GridGetHorzIJIndex — deprecated stub, always fails
   ! ---------------------------------------------------------------------------

   subroutine grid_get_horz_ij_index_stub(npts, ii, jj, lon, lat, lonR8, latR8, grid, rc)
      integer, intent(in) :: npts
      integer, intent(inout) :: ii(npts)
      integer, intent(inout) :: jj(npts)
      real, optional, intent(in) :: lon(npts)
      real, optional, intent(in) :: lat(npts)
      real(kind=ESMF_KIND_R8), optional, intent(in) :: lonR8(npts)
      real(kind=ESMF_KIND_R8), optional, intent(in) :: latR8(npts)
      type(ESMF_Grid), optional, intent(inout) :: grid
      integer, optional, intent(out) :: rc

      ii = -1
      jj = -1
      _FAIL("Use GeomGetHorzIJIndex instead")

      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
      _UNUSED_DUMMY(lonR8)
      _UNUSED_DUMMY(latR8)
      _UNUSED_DUMMY(grid)
   end subroutine grid_get_horz_ij_index_stub

end module mapl_GeomAccessors

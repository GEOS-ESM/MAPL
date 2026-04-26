#include "MAPL.h"

module mapl3g_EASEGeomSpec

   use mapl3g_GeomSpec
   use mapl3g_EASEConversion
   use mapl_ErrorHandlingMod
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use, intrinsic :: iso_fortran_env, only: REAL64

   implicit none
   private

   public :: EASEGeomSpec
   public :: make_EASEGeomSpec

   ! EASEGeomSpec holds the grid name (e.g. 'EASEv2_M09'), the global
   ! grid dimensions derived from it, and precomputed 1-D coordinate
   ! arrays (in degrees) for grid-cell centers and corners.
   !
   ! Convention (matching legacy EASEGridFactory):
   !   - Longitude runs -180 to +180 with the dateline at the grid edge ('DE')
   !   - Latitude runs south to north (index 1 = southernmost row)
   !   - No poles are included ('XY')
   !   - Coordinates stored in degrees
   !
   type, extends(GeomSpec) :: EASEGeomSpec
      private
      character(len=:), allocatable :: grid_name   ! e.g. 'EASEv2_M09'
      integer :: im_world = 0                       ! number of columns (lons)
      integer :: jm_world = 0                       ! number of rows    (lats)
      real(kind=REAL64), allocatable :: lon_centers(:)  ! degrees, size im_world
      real(kind=REAL64), allocatable :: lat_centers(:)  ! degrees, size jm_world
      real(kind=REAL64), allocatable :: lon_corners(:)  ! degrees, size im_world+1
      real(kind=REAL64), allocatable :: lat_corners(:)  ! degrees, size jm_world+1
   contains
      ! Mandatory GeomSpec interface
      procedure :: equal_to
      procedure :: get_horz_ij_index_r4
      procedure :: get_horz_ij_index_r8

      ! EASE-specific: dispatch support predicates
      procedure :: supports_hconfig  => supports_hconfig_
      procedure :: supports_metadata => supports_metadata_
      generic :: supports => supports_hconfig, supports_metadata

      ! Accessors
      procedure :: get_grid_name
      procedure :: get_im_world
      procedure :: get_jm_world
      procedure :: get_lon_centers
      procedure :: get_lat_centers
      procedure :: get_lon_corners
      procedure :: get_lat_corners
   end type EASEGeomSpec

   interface EASEGeomSpec
      module procedure new_EASEGeomSpec
   end interface EASEGeomSpec

   interface make_EASEGeomSpec
      procedure make_EASEGeomSpec_from_hconfig
      procedure make_EASEGeomSpec_from_metadata
   end interface make_EASEGeomSpec

   integer, parameter :: R4 = ESMF_KIND_R4
   integer, parameter :: R8 = ESMF_KIND_R8

interface

   pure logical module function equal_to(a, b)
      class(EASEGeomSpec), intent(in) :: a
      class(GeomSpec),     intent(in) :: b
   end function equal_to

   module function make_EASEGeomSpec_from_hconfig(hconfig, rc) result(spec)
      use esmf, only: ESMF_HConfig
      type(EASEGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
   end function make_EASEGeomSpec_from_hconfig

   module function make_EASEGeomSpec_from_metadata(file_metadata, rc) result(spec)
      use pfio, only: FileMetadata
      type(EASEGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc
   end function make_EASEGeomSpec_from_metadata

   logical module function supports_hconfig_(this, hconfig, rc) result(supports)
      use esmf, only: ESMF_HConfig
      class(EASEGeomSpec), intent(in) :: this
      type(ESMF_HConfig),  intent(in) :: hconfig
      integer, optional,   intent(out) :: rc
   end function supports_hconfig_

   logical module function supports_metadata_(this, file_metadata, rc) result(supports)
      use pfio, only: FileMetadata
      class(EASEGeomSpec), intent(in) :: this
      type(FileMetadata),  intent(in) :: file_metadata
      integer, optional,   intent(out) :: rc
   end function supports_metadata_

   module subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(EASEGeomSpec), intent(in)  :: this
      real(kind=R4),       intent(in)  :: lon(:)
      real(kind=R4),       intent(in)  :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional,   intent(out) :: rc
   end subroutine get_horz_ij_index_r4

   module subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(EASEGeomSpec), intent(in)  :: this
      real(kind=R8),       intent(in)  :: lon(:)
      real(kind=R8),       intent(in)  :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional,   intent(out) :: rc
   end subroutine get_horz_ij_index_r8

end interface

contains

   ! Basic constructor: given a pre-validated grid_name, compute all
   ! coordinate arrays and fill the spec.
   function new_EASEGeomSpec(grid_name, rc) result(spec)
      type(EASEGeomSpec) :: spec
      character(len=*), intent(in) :: grid_name
      integer, optional, intent(out) :: rc

      integer :: status, cols, rows, row
      real    :: lat, tmplon, s
      real(kind=REAL64) :: delta

      call ease_extent(grid_name, cols, rows, rc=status)
      _VERIFY(status)

      spec%grid_name = grid_name
      spec%im_world  = cols
      spec%jm_world  = rows

      ! Longitude centers: uniformly spaced from -180+delta/2 to +180-delta/2
      allocate(spec%lon_centers(cols))
      allocate(spec%lon_corners(cols+1))
      delta = 360.0_REAL64 / cols
      block
         integer :: i
         do i = 1, cols
            spec%lon_centers(i) = -180.0_REAL64 + (i - 0.5_REAL64)*delta
         end do
         do i = 1, cols+1
            spec%lon_corners(i) = -180.0_REAL64 + (i - 1)*delta
         end do
      end block

      ! Latitude centers: computed via EASE inverse (0-based row index)
      ! EASE counts rows from North to South; we store index 1 = southernmost.
      allocate(spec%lat_centers(rows))
      allocate(spec%lat_corners(rows+1))
      do row = 0, rows-1
         s = real(row)
         call ease_inverse(grid_name, 0., s, lat, tmplon)
         spec%lat_centers(rows - row) = real(lat, kind=REAL64)
      end do

      ! Latitude corners (fractional indices offset by 0.5)
      do row = 0, rows
         s = real(row) - 0.5
         call ease_inverse(grid_name, 0., s, lat, tmplon)
         spec%lat_corners(rows + 1 - row) = real(lat, kind=REAL64)
      end do

      _RETURN(_SUCCESS)
   end function new_EASEGeomSpec

   ! Accessors
   pure function get_grid_name(this) result(name)
      class(EASEGeomSpec), intent(in) :: this
      character(len=:), allocatable :: name
      name = this%grid_name
   end function get_grid_name

   pure integer function get_im_world(this)
      class(EASEGeomSpec), intent(in) :: this
      get_im_world = this%im_world
   end function get_im_world

   pure integer function get_jm_world(this)
      class(EASEGeomSpec), intent(in) :: this
      get_jm_world = this%jm_world
   end function get_jm_world

   pure function get_lon_centers(this) result(lons)
      class(EASEGeomSpec), intent(in) :: this
      real(kind=REAL64), allocatable :: lons(:)
      lons = this%lon_centers
   end function get_lon_centers

   pure function get_lat_centers(this) result(lats)
      class(EASEGeomSpec), intent(in) :: this
      real(kind=REAL64), allocatable :: lats(:)
      lats = this%lat_centers
   end function get_lat_centers

   pure function get_lon_corners(this) result(lons)
      class(EASEGeomSpec), intent(in) :: this
      real(kind=REAL64), allocatable :: lons(:)
      lons = this%lon_corners
   end function get_lon_corners

   pure function get_lat_corners(this) result(lats)
      class(EASEGeomSpec), intent(in) :: this
      real(kind=REAL64), allocatable :: lats(:)
      lats = this%lat_corners
   end function get_lat_corners

end module mapl3g_EASEGeomSpec

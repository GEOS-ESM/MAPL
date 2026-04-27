#include "MAPL.h"

module mapl3g_EASEGeomSpec

   use mapl3g_GeomSpec
   use mapl3g_EASEConversion
   use mapl_ErrorHandlingMod
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8

   implicit none
   private

   public :: EASEGeomSpec
   public :: make_EASEGeomSpec

   ! EASEGeomSpec holds only the grid name (e.g. 'EASEv2_M09').
   ! Grid dimensions are derived on demand from the grid name via ease_extent.
   ! Coordinate arrays are computed by the factory and not stored here.
   !
   type, extends(GeomSpec) :: EASEGeomSpec
      private
      character(len=:), allocatable :: grid_name   ! e.g. 'EASEv2_M09'
   contains
      ! Mandatory GeomSpec interface
      procedure :: equal_to
      procedure :: get_horz_ij_index_r4
      procedure :: get_horz_ij_index_r8

      ! EASE-specific: dispatch support predicates
      procedure :: supports_hconfig  => supports_hconfig_
      procedure :: supports_metadata => supports_metadata_
      generic :: supports => supports_hconfig, supports_metadata

      ! Methods (compute from grid_name on demand)
      procedure :: get_grid_name
      procedure :: get_im_world
      procedure :: get_jm_world
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

   function new_EASEGeomSpec(grid_name, rc) result(spec)
      type(EASEGeomSpec) :: spec
      character(len=*), intent(in) :: grid_name
      integer, optional, intent(out) :: rc

      integer :: status, cols, rows

      ! Validate grid_name by calling ease_extent
      call ease_extent(grid_name, cols, rows, rc=status)
      _VERIFY(status)

      spec%grid_name = grid_name
      _RETURN(_SUCCESS)
   end function new_EASEGeomSpec

   pure function get_grid_name(this) result(name)
      class(EASEGeomSpec), intent(in) :: this
      character(len=:), allocatable :: name
      name = this%grid_name
   end function get_grid_name

   integer function get_im_world(this, rc)
      class(EASEGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status, cols, rows
      call ease_extent(this%grid_name, cols, rows, rc=status)
      _VERIFY(status)
      get_im_world = cols
      _RETURN(_SUCCESS)
   end function get_im_world

   integer function get_jm_world(this, rc)
      class(EASEGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status, cols, rows
      call ease_extent(this%grid_name, cols, rows, rc=status)
      _VERIFY(status)
      get_jm_world = rows
      _RETURN(_SUCCESS)
   end function get_jm_world

end module mapl3g_EASEGeomSpec

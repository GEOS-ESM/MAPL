#include "MAPL.h"

module mapl3g_XYGeomSpec

   use mapl3g_GeomSpec
   use MAPL_InternalConstants
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_HConfig

   implicit none
   private

   public :: XYGeomSpec
   public :: make_XYGeomSpec

   ! Coordinate mode: whether coordinates come from a standard XY file
   ! (2D lon/lat arrays) or an ABI/GOES file (1D scan-angle arrays +
   ! satellite longitude attribute).
   integer, public, parameter :: XY_COORD_STANDARD = 0
   integer, public, parameter :: XY_COORD_ABI      = 1

   type, extends(GeomSpec) :: XYGeomSpec
      private
      ! Grid file
      character(len=:), allocatable :: grid_file_name
      ! Global dimensions
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
      integer :: lm       = MAPL_UNDEFINED_INTEGER
      ! Decomposition (counts per DE in each dimension)
      integer, allocatable :: ims(:)   ! length nx
      integer, allocatable :: jms(:)   ! length ny
      ! Whether the file contains corner coordinates
      logical :: has_corners = .false.
      ! Coordinate mode
      integer :: coord_mode = XY_COORD_STANDARD
      ! ABI-specific metadata
      integer :: thin_factor   = 1
      integer :: xdim_true     = MAPL_UNDEFINED_INTEGER
      integer :: ydim_true     = MAPL_UNDEFINED_INTEGER
      character(len=:), allocatable :: index_name_x
      character(len=:), allocatable :: index_name_y
      character(len=:), allocatable :: var_name_x
      character(len=:), allocatable :: var_name_y
      character(len=:), allocatable :: var_name_proj
      character(len=:), allocatable :: att_name_proj
   contains
      ! Mandatory GeomSpec interface
      procedure :: equal_to
      procedure :: get_horz_ij_index_r4
      procedure :: get_horz_ij_index_r8

      ! XY-specific supports checks
      procedure :: supports_hconfig  => supports_hconfig_
      procedure :: supports_metadata => supports_metadata_
      generic   :: supports => supports_hconfig, supports_metadata

      ! Accessors
      procedure :: get_im_world
      procedure :: get_jm_world
      procedure :: get_lm
      procedure :: get_ims
      procedure :: get_jms
      procedure :: get_grid_file_name
      procedure :: get_has_corners
      procedure :: get_coord_mode
      procedure :: get_thin_factor
      procedure :: get_xdim_true
      procedure :: get_ydim_true
      procedure :: get_var_name_x
      procedure :: get_var_name_y
      procedure :: get_var_name_proj
      procedure :: get_att_name_proj
   end type XYGeomSpec

   interface XYGeomSpec
      module procedure new_XYGeomSpec
   end interface XYGeomSpec

   interface make_XYGeomSpec
      procedure make_XYGeomSpec_from_hconfig
      procedure make_XYGeomSpec_from_metadata
   end interface make_XYGeomSpec

   integer, parameter :: R4 = ESMF_KIND_R4
   integer, parameter :: R8 = ESMF_KIND_R8

interface

   pure logical module function equal_to(a, b)
      class(XYGeomSpec), intent(in) :: a
      class(GeomSpec),   intent(in) :: b
   end function equal_to

   module function make_XYGeomSpec_from_hconfig(hconfig, rc) result(spec)
      use esmf, only: ESMF_HConfig
      type(XYGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
   end function make_XYGeomSpec_from_hconfig

   module function make_XYGeomSpec_from_metadata(file_metadata, rc) result(spec)
      use pfio, only: FileMetadata
      type(XYGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc
   end function make_XYGeomSpec_from_metadata

   logical module function supports_hconfig_(this, hconfig, rc) result(supports)
      class(XYGeomSpec), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
   end function supports_hconfig_

   logical module function supports_metadata_(this, file_metadata, rc) result(supports)
      use pfio, only: FileMetadata
      class(XYGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc
   end function supports_metadata_

   module subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(XYGeomSpec), intent(in) :: this
      real(R4), intent(in) :: lon(:)
      real(R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc
   end subroutine get_horz_ij_index_r4

   module subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(XYGeomSpec), intent(in) :: this
      real(R8), intent(in) :: lon(:)
      real(R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc
   end subroutine get_horz_ij_index_r8

end interface

contains

   function new_XYGeomSpec( &
        grid_file_name, im_world, jm_world, lm, ims, jms, &
        has_corners, coord_mode, thin_factor, xdim_true, ydim_true, &
        index_name_x, index_name_y, &
        var_name_x, var_name_y, var_name_proj, att_name_proj) result(spec)
      type(XYGeomSpec) :: spec
      character(len=*),           intent(in) :: grid_file_name
      integer,                    intent(in) :: im_world
      integer,                    intent(in) :: jm_world
      integer,          optional, intent(in) :: lm
      integer,          optional, intent(in) :: ims(:)
      integer,          optional, intent(in) :: jms(:)
      logical,          optional, intent(in) :: has_corners
      integer,          optional, intent(in) :: coord_mode
      integer,          optional, intent(in) :: thin_factor
      integer,          optional, intent(in) :: xdim_true
      integer,          optional, intent(in) :: ydim_true
      character(len=*), optional, intent(in) :: index_name_x
      character(len=*), optional, intent(in) :: index_name_y
      character(len=*), optional, intent(in) :: var_name_x
      character(len=*), optional, intent(in) :: var_name_y
      character(len=*), optional, intent(in) :: var_name_proj
      character(len=*), optional, intent(in) :: att_name_proj

      spec%grid_file_name = grid_file_name
      spec%im_world       = im_world
      spec%jm_world       = jm_world
      if (present(lm))          spec%lm          = lm
      if (present(ims))         spec%ims         = ims
      if (present(jms))         spec%jms         = jms
      if (present(has_corners)) spec%has_corners  = has_corners
      if (present(coord_mode))  spec%coord_mode   = coord_mode
      if (present(thin_factor)) spec%thin_factor  = thin_factor
      if (present(xdim_true))   spec%xdim_true    = xdim_true
      if (present(ydim_true))   spec%ydim_true    = ydim_true
      if (present(index_name_x))  spec%index_name_x  = index_name_x
      if (present(index_name_y))  spec%index_name_y  = index_name_y
      if (present(var_name_x))    spec%var_name_x    = var_name_x
      if (present(var_name_y))    spec%var_name_y    = var_name_y
      if (present(var_name_proj)) spec%var_name_proj = var_name_proj
      if (present(att_name_proj)) spec%att_name_proj = att_name_proj

   end function new_XYGeomSpec

   ! --- Accessors ---

   integer function get_im_world(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%im_world
   end function get_im_world

   integer function get_jm_world(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%jm_world
   end function get_jm_world

   integer function get_lm(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%lm
   end function get_lm

   function get_ims(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      integer, allocatable :: v(:)
      v = this%ims
   end function get_ims

   function get_jms(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      integer, allocatable :: v(:)
      v = this%jms
   end function get_jms

   function get_grid_file_name(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      character(len=:), allocatable :: v
      v = this%grid_file_name
   end function get_grid_file_name

   logical function get_has_corners(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%has_corners
   end function get_has_corners

   integer function get_coord_mode(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%coord_mode
   end function get_coord_mode

   integer function get_thin_factor(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%thin_factor
   end function get_thin_factor

   integer function get_xdim_true(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%xdim_true
   end function get_xdim_true

   integer function get_ydim_true(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      v = this%ydim_true
   end function get_ydim_true

   function get_var_name_x(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      character(len=:), allocatable :: v
      v = this%var_name_x
   end function get_var_name_x

   function get_var_name_y(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      character(len=:), allocatable :: v
      v = this%var_name_y
   end function get_var_name_y

   function get_var_name_proj(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      character(len=:), allocatable :: v
      v = this%var_name_proj
   end function get_var_name_proj

   function get_att_name_proj(this) result(v)
      class(XYGeomSpec), intent(in) :: this
      character(len=:), allocatable :: v
      v = this%att_name_proj
   end function get_att_name_proj

end module mapl3g_XYGeomSpec

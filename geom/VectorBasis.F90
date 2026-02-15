#include "MAPL_ErrLog.h"

module mapl3g_VectorBasis
   use esmf
   use mapl_FieldBLAS
   use mapl_FieldPointerUtilities
   use mapl_ErrorHandlingMod

   implicit none(type,external)
   private

   public :: VectorBasis
   public :: GridGetCoords
   ! Factory functions
   public :: NS_VectorBasis
   public :: GridVectorBasis

   integer, parameter :: NI = 3 ! num dims cartesian
   integer, parameter :: NJ = 2 ! num dims tangent (u,v)

   type :: VectorBasis
      type(ESMF_Field), allocatable :: elements(:,:) ! (NI,NJ)
   contains
      final :: destroy_fields
   end type VectorBasis

   interface NS_VectorBasis
      module procedure new_NS_Basis
   end interface NS_VectorBasis

   interface GridVectorBasis
      module procedure new_GridVectorBasis
   end interface GridVectorBasis

   type :: Ptr_1d
      real(kind=ESMF_KIND_R8), pointer :: ptr(:)
   end type Ptr_1d

   type :: Ptr_2d
      real(kind=ESMF_KIND_R8), pointer :: ptr(:,:)
   end type Ptr_2d

   interface GridGetCoords
      module procedure grid_get_coords_1d
      module procedure grid_get_coords_2d
      module procedure grid_get_centers
   end interface GridGetCoords

   interface GridGetCorners
      module procedure grid_get_corners
   end interface GridGetCorners

   interface

      module function new_NS_Basis(geom, rc) result(basis)
         type(VectorBasis) :: basis
         type(ESMF_Geom), intent(inout) :: geom
         integer, optional, intent(out) :: rc
      end function new_NS_Basis

      ! Valid only for grids.
      module function new_GridVectorBasis(geom, inverse, rc) result(basis)
         type(VectorBasis) :: basis
         type(ESMF_Geom), intent(inout) :: geom
         logical, optional, intent(in) :: inverse
         integer, optional, intent(out) :: rc
      end function new_GridVectorBasis

      ! Utility functions
      !------------------
      pure module function get_unit_vector( p1, p2, p3 ) result(uvect)
         real(kind=ESMF_KIND_R8), intent(in):: p1(2), p2(2), p3(2) 
         real(kind=ESMF_KIND_R8) :: uvect(3) 
      end function get_unit_vector


      module subroutine create_fields(elements, geom, rc)
         type(ESMF_Field), intent(inout) :: elements(NI,NJ)
         type(ESMF_Geom), intent(in) :: geom
         integer, optional, intent(out) :: rc
      end subroutine create_fields


      ! Geometry utilities

      pure module function mid_pt_sphere(p1, p2) result(pm)
         real(kind=ESMF_KIND_R8) , intent(in)  :: p1(2), p2(2)
         real(kind=ESMF_KIND_R8) :: pm(2)
      end function mid_pt_sphere

      pure module function latlon2xyz(sph_coord,right_hand) result(xyz_coord)
         real(kind=ESMF_KIND_R8), intent(in), dimension(2) :: sph_coord
         logical, intent(in), optional :: right_hand
         real(kind=ESMF_KIND_R8), dimension(3) :: xyz_coord
      end function latlon2xyz

      pure module function xyz2latlon(xyz_coord) result(sph_coord)
         real(kind=ESMF_KIND_R8), intent(in):: xyz_coord(3)
         real(kind=ESMF_KIND_R8) :: sph_coord(2)
      end function xyz2latlon

      module subroutine destroy_fields(this)
         type(VectorBasis), intent(inout) :: this
      end subroutine destroy_fields

      module subroutine MAPL_GeomGetCoords(geom, longitudes, latitudes, rc)
         type(ESMF_Geom), intent(in) :: geom
         real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
         real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
         integer, optional, intent(out) :: rc
      end subroutine MAPL_GeomGetCoords

      ! GridGetCoords - specific procedures
      module subroutine grid_get_coords_1d(grid, longitudes, latitudes, rc)
         type(ESMF_Grid), intent(in) :: grid
         real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
         real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
         integer, optional, intent(out) :: rc
      end subroutine grid_get_coords_1d

      module subroutine grid_get_coords_2d(grid, longitudes, latitudes, rc)
         type(ESMF_Grid), intent(in) :: grid
         real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
         real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)
         integer, optional, intent(out) :: rc
      end subroutine grid_get_coords_2d

      module subroutine grid_get_centers(grid, centers, rc)
         type(ESMF_Grid), intent(in) :: grid
         real(kind=ESMF_KIND_R8), allocatable, intent(out) :: centers(:,:,:)
         integer, optional, intent(out) :: rc
      end subroutine grid_get_centers

      module subroutine grid_get_corners(grid, corners, rc)
         type(ESMF_Grid), intent(inout) :: grid
         real(kind=ESMF_KIND_R8), allocatable, intent(out) :: corners(:,:,:)
         integer, optional, intent(out) :: rc
      end subroutine grid_get_corners
   end interface
end module mapl3g_VectorBasis



#include "MAPL_ErrLog.h"
module sf_MeshElement
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT64
   use sf_Point
   use sf_Pixel
   use sf_PixelVector
   use mapl_ErrorHandling
   implicit none(type,external)
   private

   public :: MeshElement
   public :: do_refine
   public :: get_child

   integer, parameter :: WP = REAL32

   type :: MeshElement
      real(kind=WP) :: lon_1, lon_2, lat_1, lat_2
      type(Pixel), pointer :: pixels(:,:) => null()
   end type MeshElement

contains

   logical function do_refine(element)
      type(MeshElement), intent(in) :: element

      integer :: min_idx, max_idx

      min_idx = maxval(element%pixels%catch_index)
      max_idx = minval(element%pixels%catch_index)

      do_refine = (max_idx /= min_idx)

   end function do_refine

   function area(element)
      real(kind=WP) :: area
      type(MeshElement), intent(in) :: element

      real(kind=WP) :: dlon
      associate (e => element)
        dlon = (e%lon_2 - e%lon_1) ! branch cut?
        area =  (sin(e%lat_2) - sin(e%lat_1)) * dlon
      end associate
   end function area

   function get_child(element, ix, iy, nx, ny) result(child)
      type(MeshElement) :: child
      type(MeshElement), target, intent(in) :: element
      integer, intent(in) :: ix, iy
      integer, intent(in) :: nx, ny

      integer :: shp(2)

      associate ( &
           lon_1 => element%lon_1, lon_2 => element%lon_2, &
           lat_1 => element%lat_1, lat_2 => element%lat_2 )

        child%lon_1 = lon_1 + ((lon_2-lon_1) * (ix-1))/nx
        child%lon_2 = lon_1 + ((lon_2-lon_1) * (ix+0))/nx
        child%lat_1 = lat_1 + ((lat_2-lat_1) * (iy-1))/ny
        child%lat_2 = lat_1 + ((lat_2-lat_1) * (iy+0))/ny

      end associate

      shp = shape(element%pixels)
      associate (&
           i0 => 1 + (ix-1)*shp(1)/nx, &
           i1 => 0 + (ix+0)*shp(1)/nx, &
           j0 => 1 + (iy-1)*shp(2)/ny, &
           j1 => 0 + (iy+0)*shp(2)/ny )
           
        child%pixels => element%pixels(i0:i1,j0:j1)

      end associate

   end function get_child

end module sf_MeshElement

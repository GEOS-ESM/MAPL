#include "MAPL_ErrLog.h"
module sf_MeshElement
   use, intrinsic :: iso_fortran_env, only: REAL64
   use sf_Point
   use sf_Pixel
   use sf_PixelVector
   use mapl_ErrorHandling
   implicit none(type,external)
   private

   public :: MeshElement
   public :: do_refine
   public :: get_child

   type :: MeshElement
      real(kind=REAL64) :: lon_1, lon_2, lat_1, lat_2
      type(Point) :: corners(2,2)   ! (1,1): SW,  (2,1): SE,  (1,2): NW,  (2,2): NE (counterclockwise)
      type(PixelVector) :: pixels
   end type MeshElement

contains

   logical function do_refine(element)
      type(MeshElement), intent(in) :: element

      integer :: i, min_idx, max_idx
      type(Pixel), pointer :: p

      min_idx = huge(1)
      max_idx = -1

      do i = 1, element%pixels%size()
         p => element%pixels%of(i)
         min_idx = min(min_idx, p%catch_index)
         max_idx = max(max_idx, p%catch_index)
      end do

      do_refine = (max_idx /= min_idx)

   end function do_refine

   function area(element)
      real(kind=REAL64) :: area
      type(MeshElement), intent(in) :: element

      real(kind=REAL64) :: dlon
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

      integer :: i
      type(Pixel), pointer :: p

      associate ( &
           lon_1 => element%lon_1, lon_2 => element%lon_2, &
           lat_1 => element%lat_1, lat_2 => element%lat_2 )

        child%lon_1 = lon_1 + ((lon_2-lon_1) * (ix-1))/nx
        child%lon_2 = lon_1 + ((lon_2-lon_1) * (ix+0))/nx
        child%lat_1 = lat_1 + ((lat_2-lat_1) * (iy-1))/nx
        child%lat_2 = lat_1 + ((lat_2-lat_1) * (iy+0))/nx

      end associate

      associate ( &
        a => element%corners(1,1), &
        b => element%corners(2,1), &
        c => element%corners(2,2), &
        d => element%corners(1,2) )
        
        child%corners(1,1) = linear(a, b, real(ix-1,kind=REAL64)/nx, real(iy-1,kind=REAL64)/ny)
        child%corners(1,1) = linear(a, b, real(ix+0,kind=REAL64)/nx, real(iy-1,kind=REAL64)/ny)
        child%corners(1,1) = linear(a, b, real(ix+0,kind=REAL64)/nx, real(iy+0,kind=REAL64)/ny)
        child%corners(1,1) = linear(a, b, real(ix-1,kind=REAL64)/nx, real(iy+0,kind=REAL64)/ny)

      end associate

      do i = 1, element%pixels%size()
         p => element%pixels%of(i)
         if (p%center%longitude >= child%lon_1 .and. p%center%longitude < child%lon_2) then
            if (p%center%latitude >= child%lat_1 .and. p%center%latitude < child%lat_2) then
               call child%pixels%push_back(p)
            end if
         end if
      end do

   end function get_child

!!$   subroutine make_children(element, nx, ny) result(children)
!!$      type(MeshElement) :: children(nx,ny)
!!$      type(MeshElement), intent(in) :: element
!!$      integer, intent(in) :: nx, ny
!!$
!!$      integer :: ix, jy, n
!!$
!!$      do iy = 1, ny
!!$         do ix = 1, nx
!!$            call set_bounds(e, children(ix,iy), ix, iy, nx, ny)
!!$            call set_corners(e, children(ix,iy), ix, iy, nx, ny)
!!$         end do
!!$      end do
!!$   end subroutine make_children
!!$      
end module sf_MeshElement

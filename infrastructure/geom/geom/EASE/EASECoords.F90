#include "MAPL.h"

module mapl3g_EASECoords
   ! Private helper module: coordinate computation for EASE grids.
   ! Used by EASEGeomFactory submodules; not part of the public API.

   use mapl3g_EASEConversion
   use mapl3g_EASEGeomSpec
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   use mapl_ErrorHandlingMod
   use, intrinsic :: iso_fortran_env, only: REAL64

   implicit none
   private

   public :: compute_lons
   public :: compute_lats

contains

   ! Compute 1-D longitude arrays for an EASE grid.
   ! At least one of centers or corners must be present (use keyword args).
   subroutine compute_lons(spec, unusable, centers, corners, rc)
      type(EASEGeomSpec),                        intent(in)  :: spec
      class(KE),                       optional, intent(in)  :: unusable
      real(kind=REAL64), allocatable,  optional, intent(out) :: centers(:)
      real(kind=REAL64), allocatable,  optional, intent(out) :: corners(:)
      integer,                         optional, intent(out) :: rc

      integer :: cols, i, status
      real(kind=REAL64) :: delta

      cols  = spec%get_im_world(_RC)
      delta = 360.0_REAL64 / cols

      if (present(centers)) then
         allocate(centers(cols))
         do i = 1, cols
            centers(i) = -180.0_REAL64 + (i - 0.5_REAL64)*delta
         end do
      end if

      if (present(corners)) then
         allocate(corners(cols+1))
         do i = 1, cols+1
            corners(i) = -180.0_REAL64 + (i - 1)*delta
         end do
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine compute_lons

   ! Compute 1-D latitude arrays for an EASE grid.
   ! At least one of centers or corners must be present (use keyword args).
   subroutine compute_lats(spec, unusable, centers, corners, rc)
      type(EASEGeomSpec),                        intent(in)  :: spec
      class(KE),                       optional, intent(in)  :: unusable
      real(kind=REAL64), allocatable,  optional, intent(out) :: centers(:)
      real(kind=REAL64), allocatable,  optional, intent(out) :: corners(:)
      integer,                         optional, intent(out) :: rc

      integer :: rows, row, status
      real :: lat, tmplon, s
      character(len=:), allocatable :: grid_name

      grid_name = spec%get_grid_name()
      rows      = spec%get_jm_world(_RC)

      if (present(centers)) then
         allocate(centers(rows))
         do row = 0, rows-1
            s = real(row)
            call ease_inverse(grid_name, 0., s, lat, tmplon)
            centers(rows - row) = real(lat, kind=REAL64)
         end do
      end if

      if (present(corners)) then
         allocate(corners(rows+1))
         do row = 0, rows
            s = real(row) - 0.5
            call ease_inverse(grid_name, 0., s, lat, tmplon)
            corners(rows + 1 - row) = real(lat, kind=REAL64)
         end do
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine compute_lats

end module mapl3g_EASECoords

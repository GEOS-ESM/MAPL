#include "MAPL_Generic.h"

module mapl3g_tmp

   ! NOTE:
   ! The enclosed routine should probably be a part of ModelVerticalGrid
   
   use mapl_ErrorHandling
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: compute_centered_var_from_edge

contains

   subroutine compute_centered_var_from_edge(edge_var, centered_var)
      ! NOTE: centered_var is always 1-based
      real(REAL32), intent(in) :: edge_var(:)
      real(REAL32), allocatable, intent(out) :: centered_var(:)

      integer :: top, bottom

      top = lbound(edge_var, 1)
      bottom = ubound(edge_var, 1)

      centered_var = 0.5 * (edge_var(top+1:bottom) + edge_var(top:bottom-1))
   end subroutine compute_centered_var_from_edge

end module mapl3g_tmp

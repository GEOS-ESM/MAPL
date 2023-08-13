#include "MAPL_ErrLog.h"

module mapl3g_Coordinates1D
   implicit none
   private

   public :: Coordinates1D
   
   type :: Coordinates1D
      logical :: is_regular = .false.
      real(kind=REAL64), allocatable :: lon_centers(:)
      real(kind=REAL64), allocatable :: lat_centers(:)
      real(kind=REAL64), allocatable :: lon_centers_degrees(:)
      real(kind=REAL64), allocatable :: lat_centers_degrees(:)
      real(kind=REAL64), allocatable :: lon_corners(:)
      real(kind=REAL64), allocatable :: lat_corners(:)
   end type Coordinates1D
   
end module mapl3g_Coordinates1D

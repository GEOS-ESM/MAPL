#include "MAPL_ErrLog.h"

module mapl3g_GeomResolution2D
   implicit none
   private

   public :: GeomResolution2D

   type :: GeomResolution2D
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
   end type GeomResolution2D
   
end module mapl3g_GeomResolution2D

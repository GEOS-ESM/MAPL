#include "MAPL.h"

module mapl3g_CouplerPhases

   implicit none
   private

   ! Phase indices
   public :: GENERIC_COUPLER_INITIALIZE
   public :: GENERIC_COUPLER_UPDATE
   public :: GENERIC_COUPLER_INVALIDATE
   public :: GENERIC_COUPLER_CLOCK_ADVANCE

   enum, bind(c)
      enumerator :: GENERIC_COUPLER_INITIALIZE = 1
      enumerator :: GENERIC_COUPLER_UPDATE
      enumerator :: GENERIC_COUPLER_INVALIDATE
      enumerator :: GENERIC_COUPLER_CLOCK_ADVANCE
   end enum

end module mapl3g_CouplerPhases

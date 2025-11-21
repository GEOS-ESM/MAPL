! This module provides (some) backward compatibility for MAPL2
! GridComps.  Not all MAPL2 interfaces are supported.

#include "MAPL.h"

module mapl3g_Deprecated
   use mapl3g_Generic, only: MAPL_Get => MAPL_GridCompGet
   implicit none
   private

   public :: MAPL_Get
   
end module mapl3g_Deprecated

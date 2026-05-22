! This module provides (some) backward compatibility for MAPL2
! GridComps.  Not all MAPL2 interfaces are supported.

#include "MAPL.h"

module mapl_Deprecated
   use mapl_Generic, only: MAPL_Get => MAPL_GridCompGet
   implicit none(type,external)
   private

   public :: MAPL_Get
   
end module mapl_Deprecated

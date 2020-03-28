#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_ioClientsMod
   use MAPL_StringClientManagerMapMod
   implicit none
   private

   public :: iclient_managers_map
   public :: oclient_managers_map

   type(StringClientManagerMap), target :: iclient_managers_map
   type(StringClientManagerMap), target :: oclient_managers_map

end module MAPL_ioClientsMod

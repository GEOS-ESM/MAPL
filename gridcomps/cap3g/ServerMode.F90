#include "MAPL.h"

module mapl3g_ServerMode
   use mapl3g_ApplicationMode
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: ServerMode

   type, extends(ApplicationMode) :: ServerMode
   contains
      procedure :: run
   end type ModelMode

contains

end module mapl3g_ServerMode

#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) read_restart_smod
   implicit none

contains

   module subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc


      _RETURN(ESMF_SUCCESS)
   end subroutine read_restart

end submodule read_restart_smod

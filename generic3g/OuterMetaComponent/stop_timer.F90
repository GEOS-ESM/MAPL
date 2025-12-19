#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) stop_timer_smod

   use mapl_ErrorHandling

   implicit none

contains

   module subroutine stop_timer(this, name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status

      call this%profiler%stop(name, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine stop_timer

end submodule stop_timer_smod

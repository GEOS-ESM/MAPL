#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) start_timer_smod

   use mapl_ErrorHandling

   implicit none

contains

   module subroutine start_timer(this, name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status

      call this%profiler%start(name, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine start_timer

end submodule start_timer_smod

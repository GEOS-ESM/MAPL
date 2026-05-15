#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) clock_advance_smod
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   module subroutine clock_advance(this, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_ClockAdvance(this%clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine clock_advance

end submodule clock_advance_smod

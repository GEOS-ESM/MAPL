#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_geom_smod

   use mapl_ErrorHandling
   implicit none

contains

   module function get_geom(this, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(out), optional :: rc

      geom = this%geom

      _RETURN(_SUCCESS)
   end function get_geom

end submodule get_geom_smod

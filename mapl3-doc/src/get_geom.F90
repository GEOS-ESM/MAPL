#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) get_geom_smod
   implicit none

contains

   module function get_geom(this) result(geom)
      type(ESMF_Geom) :: geom
      class(OuterMetaComponent), intent(inout) :: this

      geom = this%geom

   end function get_geom

end submodule get_geom_smod

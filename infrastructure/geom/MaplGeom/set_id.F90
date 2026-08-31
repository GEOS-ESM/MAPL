#include "MAPL.h"

submodule (mapl_MaplGeom_mod) set_id_smod
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod

contains

   module subroutine set_id(this, id, rc)
      class(MaplGeom), intent(inout) :: this
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status

      call GeomSetId(this%geom, id, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_id

end submodule set_id_smod

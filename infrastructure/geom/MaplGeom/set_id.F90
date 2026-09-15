#include "MAPL.h"

submodule (mapl_MaplGeom_mod) set_id_smod
   use mapl_GeomId_mod, only: GeomId
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod

contains

   module subroutine set_id(this, id, rc)
      class(MaplGeom), intent(inout) :: this
       type(GeomId), intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status

       this%geom_id = id
       call GeomSetId(this%geom, id%get_value(), _RC)

      _RETURN(_SUCCESS)
   end subroutine set_id

end submodule set_id_smod

#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) set_geom_smod
   use mapl_geom_api, only: mapl_GeomIdManager
   use mapl_geom_api, only: MAPL_GeomGetId
   use mapl_geom_api, only: mapl_get_geom_id_manager
   use mapl_geom_api, only: mapl_new_GeomId
   implicit none(type,external)

contains

   module subroutine set_geom(this, geom)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom

      type(mapl_GeomIdManager), pointer :: geom_id_manager
      logical :: has_geom_id
      integer :: geom_id_value

      if (.not. this%geom_id%is_assigned()) then
         geom_id_value = MAPL_GeomGetId(geom, isPresent=has_geom_id)
         if (has_geom_id) then
            this%geom_id = mapl_new_GeomId(geom_id_value)
         else
            geom_id_manager => mapl_get_geom_id_manager()
            this%geom_id = geom_id_manager%get_next_geom_id()
         end if
      end if

      this%geom = geom

   end subroutine set_geom

end submodule set_geom_smod

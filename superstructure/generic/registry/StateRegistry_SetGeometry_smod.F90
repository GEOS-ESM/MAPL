#include "MAPL.h"

submodule (mapl_StateRegistry_mod) StateRegistry_SetGeometry_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine set_geometry(this, geom, vertical_grid, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(VirtualPtFamilyMapIterator) :: family_iter
      type(ExtensionFamily), pointer :: family
      type(StateItemSpec), pointer :: primary

      associate (e => this%family_map%ftn_end())
         family_iter = this%family_map%ftn_begin()
         do while (family_iter /= e)
            call family_iter%next()
            family => family_iter%second()
            if (family%has_primary()) then
               primary => family%get_primary(_RC)
               call primary%set_geometry(geom=geom, vertical_grid=vertical_grid, _RC)
            end if
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine set_geometry

end submodule StateRegistry_SetGeometry_smod

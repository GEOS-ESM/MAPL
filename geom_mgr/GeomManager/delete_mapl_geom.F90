#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) delete_mapl_geom_smod
   use mapl3g_GeomSpec
   use mapl3g_NullGeomSpec
   use mapl3g_MaplGeom
   use mapl3g_GeomFactory
   use mapl3g_GeomFactoryVector
   use mapl3g_GeomSpecVector
   use mapl3g_IntegerMaplGeomMap
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod
   use esmf
   use gftl2_IntegerVector
   implicit none

contains
   
   module subroutine delete_mapl_geom(this, geom_spec, rc)
      class(GeomManager), intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: id, idx
      integer :: n

      associate (specs => this%geom_specs)

        associate (spec_iter => find(specs%begin(), specs%end(), geom_spec))
          if (spec_iter /= specs%end()) then

             idx = 1 + (spec_iter - specs%begin())
             id = this%geom_ids%of(idx)

             n = this%mapl_geoms%erase(id) ! num deleted
             _ASSERT(n == 1, "Inconsistent status in GeomManager.")

             _RETURN(_SUCCESS)
          end if
        end associate
      end associate

      _FAIL('GeomSpec not found.')

   end subroutine delete_mapl_geom

end submodule delete_mapl_geom_smod

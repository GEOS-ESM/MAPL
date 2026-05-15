#include "MAPL.h"

submodule (mapl3g_GeomManager) get_mapl_geom_from_spec_smod

   implicit none

contains
   
   module function get_mapl_geom_from_spec(this, geom_spec, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomSpecVectorIterator) :: iter
      integer :: idx

      associate (b => this%geom_specs%begin(), e => this%geom_specs%end())
        iter = find(first=b, last=e, value=geom_spec)
        if (iter /= this%geom_specs%end()) then
           idx = iter - b + 1  ! Fortran index starts at 1
           mapl_geom => this%mapl_geoms%at(idx, _RC)
           _RETURN(_SUCCESS)
        end if
      end associate

      ! Otherwise build a new geom and store it.
      mapl_geom => this%add_mapl_geom(geom_spec, _RC)
      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_spec

end submodule get_mapl_geom_from_spec_smod

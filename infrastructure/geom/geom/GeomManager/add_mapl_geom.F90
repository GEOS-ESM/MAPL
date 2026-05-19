#include "MAPL.h"

submodule (mapl3g_GeomManager) add_mapl_geom_smod

   implicit none

contains
   

   ! Add a new mapl_geom given a geom_spec.
   ! This also labels the geom with a unique id using ESMF_Info.
   module function add_mapl_geom(this, geom_spec, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom) :: tmp_mapl_geom
      type(GeomSpecVectorIterator) :: iter

      mapl_geom => null() ! unless

      associate (b => this%geom_specs%begin(), e => this%geom_specs%end())
        iter = find(b, e, geom_spec)
        _ASSERT(iter == e, "Requested geom_spec already exists.")
      end associate

      tmp_mapl_geom = this%make_mapl_geom(geom_spec, _RC)

      associate (id => this%id_counter)
        id = id + 1
        _ASSERT(id <= MAX_ID, "Too many geoms created.")

        call tmp_mapl_geom%set_id(id, _RC)
        call this%geom_ids%push_back(id)
        call this%geom_specs%push_back(geom_spec)
        call this%mapl_geoms%insert(id, tmp_mapl_geom)

        mapl_geom => this%mapl_geoms%of(id)
      end associate

      _RETURN(_SUCCESS)
   end function add_mapl_geom

end submodule add_mapl_geom_smod

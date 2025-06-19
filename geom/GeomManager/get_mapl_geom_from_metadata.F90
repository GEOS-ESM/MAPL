#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) get_mapl_geom_from_metadata_smod
   implicit none(type,external)

contains
   
   module function get_mapl_geom_from_metadata(this, metadata, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      class(GeomSpec), allocatable :: geom_spec
      integer :: status

      geom_spec = this%make_geom_spec(metadata, _RC)
      mapl_geom => this%get_mapl_geom(geom_spec, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_metadata

end submodule get_mapl_geom_from_metadata_smod

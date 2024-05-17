#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) make_geom_spec_from_metadata_smod

   implicit none

contains
   
   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: status

      geom_spec = NullGeomSpec()
      factory => find_factory(this%factories, supports_metadata, _RC)
      geom_spec = factory%make_spec(file_metadata, _RC)
      
      _RETURN(_SUCCESS)
   contains
      logical function supports_metadata(factory)
         class(GeomFactory), intent(in) :: factory
         supports_metadata = factory%supports(file_metadata)
      end function supports_metadata
   end function make_geom_spec_from_metadata

end submodule make_geom_spec_from_metadata_smod

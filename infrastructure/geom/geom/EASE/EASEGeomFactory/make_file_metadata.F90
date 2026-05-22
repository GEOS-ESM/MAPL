#include "MAPL.h"

submodule (mapl_EASEGeomFactory) make_file_metadata_smod
   use mapl_GeomSpec_mod
   use mapl_EASEGeomSpec_mod
   use mapl_ErrorHandling_mod
   use pfio
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   implicit none (type, external)

contains

   module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(EASEGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: status

      file_metadata = FileMetadata()

      select type (geom_spec)
      type is (EASEGeomSpec)
         file_metadata = typesafe_make_file_metadata(geom_spec, chunksizes=chunksizes, _RC)
      class default
         _FAIL('make_file_metadata: geom_spec is not of dynamic type EASEGeomSpec')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
   end function make_file_metadata

end submodule make_file_metadata_smod

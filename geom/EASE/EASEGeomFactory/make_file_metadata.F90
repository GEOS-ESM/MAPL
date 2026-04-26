#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) make_file_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl_ErrorHandlingMod
   use pfio
   use mapl_KeywordEnforcer, only: KeywordEnforcer
   implicit none (type, external)

contains

   module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(EASEGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
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

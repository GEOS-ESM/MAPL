#include "MAPL.h"

submodule (mapl_GeomManager_mod) make_mapl_geom_from_spec_smod

   implicit none

contains
   
   module function make_mapl_geom_from_spec(this, spec, rc) result(mapl_geom)
      use gftl2_StringVector
      use mapl_StringDictionary_mod
      type(MaplGeom) :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom
      type(FileMetadata) :: file_metadata
      type(StringVector) :: gridded_dims
      type(StringDictionary) :: variable_attributes

      geom = spec%make_geom(_RC)
      file_metadata = spec%make_file_metadata(_RC)
      gridded_dims = spec%make_gridded_dims(_RC)
      variable_attributes = spec%make_variable_attributes(_RC)
      mapl_geom = MaplGeom(spec=spec, geom=geom, file_metadata=file_metadata, &
           gridded_dims=gridded_dims, variable_attributes=variable_attributes)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_mapl_geom_from_spec

end submodule make_mapl_geom_from_spec_smod

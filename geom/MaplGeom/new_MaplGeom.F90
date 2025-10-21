#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) new_MaplGeom_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function new_MaplGeom(spec, geom, factory, file_metadata, gridded_dims, variable_attributes) result(mapl_geom)
      class(GeomSpec), intent(in) :: spec
      type(MaplGeom) :: mapl_geom
      type(ESMF_Geom), intent(in) :: geom
      class(GeomFactory), intent(in) :: factory
      type(FileMetadata), optional, intent(in) :: file_metadata
      type(StringVector), optional, intent(in) :: gridded_dims
      type(StringStringMap), optional, intent(in) :: variable_attributes

      mapl_geom%spec = spec
      mapl_geom%geom = geom
      mapl_geom%factory = factory
      if (present(file_metadata)) mapl_geom%file_metadata = file_metadata
      if (present(gridded_dims)) mapl_geom%gridded_dims = gridded_dims
      if (present(variable_attributes)) mapl_geom%variable_attributes = variable_attributes

   end function new_MaplGeom

end submodule new_MaplGeom_smod

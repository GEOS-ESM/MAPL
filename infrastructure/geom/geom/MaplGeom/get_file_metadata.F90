#include "MAPL.h"

submodule (mapl_MaplGeom) get_file_metadata_smod
   use mapl_GeomSpec_mod
   use mapl_VectorBasis_mod
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function get_file_metadata(this) result(file_metadata)
     type(FileMetadata) :: file_metadata
      class(MaplGeom), intent(in) :: this
      file_metadata = this%file_metadata
   end function get_file_metadata

end submodule get_file_metadata_smod

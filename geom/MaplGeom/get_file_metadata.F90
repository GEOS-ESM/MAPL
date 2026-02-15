#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) get_file_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
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

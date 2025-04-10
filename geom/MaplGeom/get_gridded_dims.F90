#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) get_gridded_dims_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function get_gridded_dims(this) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(MaplGeom), intent(in) :: this
      gridded_dims = this%gridded_dims
   end function get_gridded_dims

end submodule get_gridded_dims_smod

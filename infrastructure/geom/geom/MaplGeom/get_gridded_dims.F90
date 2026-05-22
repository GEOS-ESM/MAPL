#include "MAPL.h"

submodule (mapl_MaplGeom) get_gridded_dims_smod
   use mapl_GeomSpec_mod
   use mapl_VectorBasis_mod
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod
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

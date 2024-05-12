#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) get_spec_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function get_spec(this) result(spec)
      class(GeomSpec), allocatable :: spec
      class(MaplGeom), intent(in) :: this
      spec = this%spec
   end function get_spec

end submodule get_spec_smod

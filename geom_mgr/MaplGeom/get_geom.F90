#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) get_geom_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function get_geom(this) result(geom)
      type(ESMF_Geom) :: geom
      class(MaplGeom), intent(in) :: this
      geom = this%geom
   end function get_geom

end submodule get_geom_smod

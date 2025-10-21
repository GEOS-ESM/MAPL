#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) get_variable_attributes_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function get_variable_attributes(this) result(variable_attributes)
      type(StringStringMap) :: variable_attributes
      class(MaplGeom), intent(in) :: this
      variable_attributes = this%variable_attributes
   end function get_variable_attributes

end submodule get_variable_attributes_smod

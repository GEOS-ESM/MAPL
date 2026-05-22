#include "MAPL.h"

submodule (mapl_MaplGeom_mod) get_variable_attributes_smod
   use mapl_GeomSpec_mod
   use mapl_VectorBasis_mod
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function get_variable_attributes(this) result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(MaplGeom), intent(in) :: this
      variable_attributes = this%variable_attributes
   end function get_variable_attributes

end submodule get_variable_attributes_smod

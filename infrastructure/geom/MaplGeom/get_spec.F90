#include "MAPL.h"

submodule (mapl_MaplGeom_mod) get_spec_smod
   use mapl_GeomSpec_mod
   use mapl_VectorBasis_mod
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod
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

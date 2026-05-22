#include "MAPL.h"

submodule (mapl_MaplGeom) get_factory_smod
   use mapl_GeomSpec_mod
   use mapl_VectorBasis_mod
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module function get_factory(this) result(factory)
     class(GeomFactory), allocatable :: factory
      class(MaplGEOM), intent(in) :: this
      factory = this%factory
   end function get_factory

end submodule get_factory_smod

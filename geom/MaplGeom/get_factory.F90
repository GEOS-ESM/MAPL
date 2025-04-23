#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) get_factory_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
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

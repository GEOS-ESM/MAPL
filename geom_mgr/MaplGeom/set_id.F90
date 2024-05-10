#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) set_id_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   module subroutine set_id(this, id, rc)
      class(MaplGeom), intent(inout) :: this
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: infoh

      call MAPL_GeomSetId(this%geom, id, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine set_id

end submodule set_id_smod

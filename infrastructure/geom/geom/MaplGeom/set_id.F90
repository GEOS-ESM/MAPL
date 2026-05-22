#include "MAPL.h"

submodule (mapl_MaplGeom_mod) set_id_smod
   use mapl_GeomSpec_mod
   use mapl_VectorBasis_mod
   use mapl_GeomUtilities_mod
   use mapl_ErrorHandling_mod
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

      call MAPL_GeomSetId(this%geom, id, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_id

end submodule set_id_smod

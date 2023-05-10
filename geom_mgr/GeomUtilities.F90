#include "MAPL_ErrLog.h"

module mapl_GeomUtilities
   use esmf
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: MAPL_GeomSetId
   public :: MAPL_GeomGetId

   character(len=*), parameter :: ID_INFO_KEY = 'mapl/geom/id'

contains
   
   subroutine MAPL_GeomSetId(geom, id, rc)
      type(ESMF_Geom), intent(inout) :: geom
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(geom, info, _RC)
      call ESMF_InfoSet(info, ID_INFO_KEY, id, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine MAPL_GeomSetId

   integer function MAPL_GeomGetId(geom, rc) result(id)
      type(ESMF_Geom), intent(inout) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(geom, info, _RC)
      call ESMF_InfoGet(info, ID_INFO_KEY, id, _RC)
      
      _RETURN(_SUCCESS)
   end function MAPL_GeomGetId


end module mapl_GeomUtilities

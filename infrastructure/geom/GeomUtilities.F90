#include "MAPL.h"

module mapl_GeomUtilities_mod
   use esmf
   use mapl_ErrorHandling_mod
   implicit none
   private

   public :: GeomSetId
   public :: GeomGetId
   public :: SameGeom

   character(len=*), parameter :: ID_INFO_KEY = 'mapl/geom/id'

   interface SameGeom
      procedure :: same_geom
   end interface SameGeom

contains
   
   subroutine GeomSetId(geom, id, rc)
      type(ESMF_Geom), intent(inout) :: geom
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(geom, info, _RC)
      call ESMF_InfoSet(info, ID_INFO_KEY, id, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine GeomSetId

   integer function GeomGetId(geom, isPresent, rc) result(id)
      type(ESMF_Geom), intent(in) :: geom
      logical, optional, intent(out) :: isPresent
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info
      integer, parameter :: NOT_FOUND = -1

      call ESMF_InfoGetFromHost(geom, info, _RC)
      call ESMF_InfoGet(info, ID_INFO_KEY, id, default=NOT_FOUND, _RC)
      if (present(isPresent)) isPresent = (id /= NOT_FOUND)

      
      _RETURN(_SUCCESS)
   end function GeomGetId

   ! For now, a grid that lacks an id is treated as different than all
   ! other grids.
   logical function same_geom(geom_a, geom_b)
      type(ESMF_Geom), intent(in) :: geom_a
      type(ESMF_Geom), intent(in) :: geom_b

      logical :: has_id_a
      logical :: has_id_b
      integer :: id_a
      integer :: id_b

      same_geom = .false. ! unless

      id_a = GeomGetId(geom_a, isPresent=has_id_a)
      id_b = GeomGetId(geom_b, isPresent=has_id_b)

      if (has_id_a .and. has_id_b) then
         same_geom = (id_a == id_b)
      end if

   end function same_geom

end module mapl_GeomUtilities_mod

#include "MAPL.h"

module mapl3g_StateGetGeom
   use esmf
   use mapl_ErrorHandling
   use mapl3g_Geom_API
   implicit none(type,external)
   private

   public :: StateGetGeom

   interface StateGetGeom
      procedure state_get_geom
   end interface StateGetGeom

contains

   subroutine state_get_geom(state, geom, rc)
      type(ESMF_State), intent(in) :: state
      type(ESMF_Geom), intent(out) :: geom
      integer, optional, intent(out) :: rc

      integer :: status, item_count, i
      character(ESMF_MAXSTR), allocatable :: item_names(:)
      type(ESMF_StateItem_Flag), allocatable :: item_types(:)
      type(ESMF_Field) :: field
      type(ESMF_Geom) :: field_geom
      type(MaplGeom), pointer :: mapl_geom

      ! 1. Get item count and assert state is non-empty
      call ESMF_StateGet(state, itemCount=item_count, _RC)
      _ASSERT(item_count > 0, 'state must contain at least one item')

      allocate(item_names(item_count), item_types(item_count))
      call ESMF_StateGet(state, itemNameList=item_names, itemTypeList=item_types, _RC)

      ! 2. Assert all items are ESMF_STATEITEM_FIELD
      do i = 1, item_count
         _ASSERT(item_types(i) == ESMF_STATEITEM_FIELD, 'all state items must be fields')
      end do

      ! 3-5. For each field: get ESMF_Geom, get MaplGeom, assert same geom as first field
      do i = 1, item_count
         call ESMF_StateGet(state, itemName=item_names(i), field=field, _RC)
         call ESMF_FieldGet(field, geom=field_geom, _RC)
         mapl_geom => get_mapl_geom(field_geom, _RC)
         if (i == 1) then
            geom = field_geom
         else
            _ASSERT(MAPL_SameGeom(field_geom, geom), 'all fields must share the same geom')
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine state_get_geom

end module mapl3g_StateGetGeom

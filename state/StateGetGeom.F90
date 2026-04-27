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

   ! Public entry point — unchanged signature.
   subroutine state_get_geom(state, geom, rc)
      type(ESMF_State), intent(in) :: state
      type(ESMF_Geom), intent(out) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      found = .false.
      call collect_geom(state, geom, found, rc=status)
      if (status /= _SUCCESS) then
         if (present(rc)) rc = status
         return
      end if
      _ASSERT(found, 'state must contain at least one item')
      _RETURN(_SUCCESS)
   end subroutine state_get_geom

   ! Private recursive worker.
   ! `geom`  — carries the reference geom once the first field is encountered.
   ! `found` — .false. until the first field is processed; guards the
   !           first-vs-subsequent comparison and the final assertion above.
   recursive subroutine collect_geom(state, geom, found, rc)
      type(ESMF_State), intent(in)    :: state
      type(ESMF_Geom),  intent(inout) :: geom
      logical,          intent(inout) :: found
      integer, optional, intent(out)  :: rc

      integer :: status, item_count, i
      character(ESMF_MAXSTR), allocatable :: item_names(:)
      type(ESMF_StateItem_Flag), allocatable :: item_types(:)
      type(ESMF_Field) :: field
      type(ESMF_Geom)  :: field_geom
      type(ESMF_State) :: nested_state
      type(MaplGeom), pointer :: mapl_geom

      call ESMF_StateGet(state, itemCount=item_count, _RC)

      ! An empty (nested) state is allowed — nothing to check here.
      if (item_count == 0) then
         _RETURN(_SUCCESS)
      end if

      allocate(item_names(item_count), item_types(item_count))
      call ESMF_StateGet(state, itemNameList=item_names, itemTypeList=item_types, _RC)

      do i = 1, item_count

         if (item_types(i) == ESMF_STATEITEM_FIELD) then

            call ESMF_StateGet(state, itemName=item_names(i), field=field, _RC)
            call ESMF_FieldGet(field, geom=field_geom, _RC)
            mapl_geom => get_mapl_geom(field_geom, _RC)

            if (.not. found) then
               geom  = field_geom
               found = .true.
            else
               _ASSERT(MAPL_SameGeom(field_geom, geom), 'all fields must share the same geom')
            end if

         else if (item_types(i) == ESMF_STATEITEM_STATE) then

            call ESMF_StateGet(state, itemName=item_names(i), nestedState=nested_state, _RC)
             call collect_geom(nested_state, geom, found, rc=status)
             if (status /= _SUCCESS) then
                if (present(rc)) rc = status
                return
             end if

         else
            _ASSERT(.false., 'state items must be fields or nested states')
         end if

      end do

      _RETURN(_SUCCESS)
   end subroutine collect_geom

end module mapl3g_StateGetGeom

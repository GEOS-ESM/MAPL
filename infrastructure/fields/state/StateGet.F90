#include "MAPL.h"

module mapl3g_StateGet
   use mapl3g_VerticalGrid_API
   use mapl3g_Field_API
   use mapl3g_UngriddedDims
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   implicit none(type,external)
   private

   public :: StateGet

   interface StateGet
      procedure state_get_status
      procedure state_get
   end interface StateGet

contains

   subroutine state_get(state, itemName, unusable, &
        field, &
        typekind, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        ungridded_dims, &
        units, standard_name, long_name, &
        allocation_status, &
        rc)

      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: itemName
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_Field), optional, intent(out) :: field
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      character(len=:), optional, allocatable, intent(out) :: units
      character(len=:), optional, allocatable, intent(out) :: standard_name
      character(len=:), optional, allocatable, intent(out) :: long_name
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      integer, optional, intenT(out) :: rc

      type(ESMF_Field) :: field_
      integer :: status

      call ESMF_StateGet(state, itemName=itemName, field=field_, _RC)
      if (present(field)) field=field_

      call MAPL_FieldGet(field_, &
           typekind=typekind, &
           num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc, &
           num_vgrid_levels=num_vgrid_levels, &
           ungridded_dims=ungridded_dims, &
           units=units, standard_name=standard_name, long_name=long_name, &
           allocation_status=allocation_status, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine state_get

   recursive subroutine state_get_status(state, itemName, itemType, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: itemName
      type(esmf_StateItem_Flag), intent(out) :: itemType
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: idx
      type(esmf_State) :: nestedState
      character(:), allocatable :: subname

      idx = index(itemName, '/')
      if (idx == 0) then
         call esmf_StateGet(state, itemName=itemName, itemType=itemType, _RC)
         _RETURN(_SUCCESS)
      end if
      subname = itemName(:idx-1)

      call esmf_StateGet(state, itemName=subName, itemType=itemType, _RC)
      _RETURN_IF(itemType == ESMF_STATEITEM_NOTFOUND)
      _ASSERT(itemType == ESMF_STATEITEM_STATE, 'nestedState not found: '//subname)

      call esmf_StateGet(state, itemName=subname, nestedState=nestedState, _RC)

      call state_get_status(nestedState, itemName=itemName(idx+1:), itemType=itemType, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine state_get_status
end module mapl3g_StateGet

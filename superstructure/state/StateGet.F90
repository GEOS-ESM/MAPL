#include "MAPL.h"

module mapl_StateGet_mod

   use mapl_vertical_grid_api
   use mapl_enums_api
   use mapl_field_api
   use mapl_UngriddedDims_mod
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use mapl_field_bundle_api, only: MAPL_FieldBundleAdd, MAPL_FieldBundleGet
   use esmf

   implicit none(type,external)
   private

   public :: StateGet

   interface StateGet
      procedure state_get_status
      procedure state_get
      procedure state_get_bundle
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
      type(mapl_VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      character(len=:), optional, allocatable, intent(out) :: units
      character(len=:), optional, allocatable, intent(out) :: standard_name
      character(len=:), optional, allocatable, intent(out) :: long_name
      type(MAPL_StateItemAllocation), optional, intent(out) :: allocation_status
      integer, optional, intent(out) :: rc

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

   ! Serialize a state into a bundle of fields; non-field/bundle items are ignored.
   ! Fields from a contained FieldBundle are renamed <bundle_name>_<field_index>.
   subroutine state_get_bundle(state, bundle, rc)
      type(ESMF_State), intent(in) :: state
      type(ESMF_FieldBundle), intent(out) :: bundle
      integer, optional, intent(out) :: rc

      type(ESMF_StateItem_Flag), allocatable :: item_types(:)
      character(len=ESMF_MAXSTR), allocatable :: item_names(:)
      type(ESMF_Field), allocatable :: field_list(:)
      type(ESMF_Field) :: field, alias
      type(ESMF_FieldBundle) :: bundle2
      character(len=:), allocatable :: item_name
      character(len=ESMF_MAXSTR) :: short_name
      integer :: item_count, idx, jdx, status

      bundle = ESMF_FieldBundleCreate(_RC)
      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(item_names(item_count), _STAT)
      allocate(item_types(item_count), _STAT)
      call ESMF_StateGet(state, itemNameList=item_names, itemTypeList=item_types, _RC)
      do idx = 1, item_count
         if (allocated(field_list)) deallocate(field_list, _STAT)
         item_name = trim(item_names(idx))
         if (item_types(idx) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, item_name, field, _RC)
            call MAPL_FieldBundleAdd(bundle, [field], _RC)
         else if (item_types(idx) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, item_name, bundle2, _RC)
            call MAPL_FieldBundleGet(bundle2, fieldList=field_list, _RC) ! addorder
            do jdx = 1, size(field_list)
               write(short_name, '(I0)') jdx
               alias = ESMF_NamedAlias(field_list(jdx), name=item_name//"_"//trim(short_name), _RC)
               call MAPL_FieldBundleAdd(bundle, [alias], _RC)
            end do
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine state_get_bundle

end module mapl_StateGet_mod

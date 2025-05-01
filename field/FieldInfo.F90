#include "MAPL_Generic.h"

module mapl3g_FieldInfo
   use mapl3g_esmf_info_keys, only: INFO_SHARED_NAMESPACE
   use mapl3g_esmf_info_keys, only: INFO_INTERNAL_NAMESPACE
   use mapl3g_InfoUtilities
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: FieldInfoGetShared
   public :: FieldInfoSetShared
   public :: FieldInfoSetInternal
   public :: FieldInfoGetInternal
   public :: FieldInfoCopyShared

   interface FieldInfoSetShared
      procedure info_field_set_shared_i4
      procedure info_field_set_shared_r4
   end interface FieldInfoSetShared

   interface FieldInfoGetShared
      procedure info_field_get_shared_i4
      procedure info_field_get_shared_r4
   end interface FieldInfoGetShared

   interface FieldInfoSetInternal
      module procedure field_info_set_internal
   end interface FieldInfoSetInternal

   interface FieldInfoGetInternal
      module procedure field_info_get_internal
   end interface

    interface FieldInfoCopyShared
      procedure :: field_info_copy_shared
   end interface FieldInfoCopyShared

   character(*), parameter :: KEY_UNITS = "/units"
   character(*), parameter :: KEY_LONG_NAME = "/long_name"
   character(*), parameter :: KEY_STANDARD_NAME = "/standard_name"
   character(*), parameter :: KEY_NUM_LEVELS = "/num_levels"
   character(*), parameter :: KEY_VERT_STAGGERLOC = "/vert_staggerloc"
   character(*), parameter :: KEY_VERT_ONLY = "/vert_only"
   character(*), parameter :: KEY_UNGRIDDED_DIMS = "/ungridded_dims"
   character(*), parameter :: KEY_IS_ACTIVE = "/is_active"

   character(*), parameter :: KEY_UNDEF_VALUE = "/undef_value"
   character(*), parameter :: KEY_MISSING_VALUE = "/missing_value"
   character(*), parameter :: KEY_FILL_VALUE = "/_FillValue"

contains

   subroutine field_info_set_internal(info, unusable, &
        namespace, &
        num_levels, vert_staggerloc, &
        ungridded_dims, &
        grid_to_field_map, &
        units, long_name, standard_name, &
        is_active, &
        rc)

      type(ESMF_Info), intent(inout) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: grid_to_field_map(:)
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      character(*), optional, intent(in) :: standard_name
      logical, optional, intent(in) :: is_active
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_Info) :: ungridded_info
      character(:), allocatable :: namespace_
      logical :: vert_only

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(ungridded_dims)) then
         ungridded_info = ungridded_dims%make_info(_RC)
         call MAPL_InfoSet(info, namespace_ // KEY_UNGRIDDED_DIMS, ungridded_info, _RC)
      end if

      if (present(grid_to_field_map)) then
         vert_only = .false.
         if (all(grid_to_field_map==0)) vert_only = .true.
         call MAPL_InfoSet(info, namespace_ // KEY_VERT_ONLY, vert_only, _RC)
      end if

      if (present(units)) then
         call MAPL_InfoSet(info, namespace_ // KEY_UNITS, units, _RC)
      end if

      if (present(long_name)) then
         call MAPL_InfoSet(info, namespace_ // KEY_LONG_NAME, long_name, _RC)
      end if

      if (present(standard_name)) then
         call MAPL_InfoSet(info, namespace_ // KEY_STANDARD_NAME, standard_name, _RC)
      end if

      if (present(num_levels)) then
         call MAPL_InfoSet(info, namespace_ // KEY_NUM_LEVELS, num_levels, _RC)
      end if

      if (present(vert_staggerloc)) then
         call MAPL_InfoSet(info, namespace_ // KEY_VERT_STAGGERLOC, vert_staggerloc%to_string(), _RC)

         ! Delete later - needed for transition

         if (present(num_levels) .and. present(vert_staggerloc)) then
            if (vert_staggerLoc == VERTICAL_STAGGER_NONE) then
               call MAPL_InfoSet(info, namespace_ // "/vertical_dim/vloc", "VERTICAL_DIM_NONE", _RC)
               call MAPL_InfoSet(info, namespace_ // "/vertical_grid/num_levels", 0, _RC)
            else if (vert_staggerLoc == VERTICAL_STAGGER_EDGE) then
               call MAPL_InfoSet(info, namespace_ // "/vertical_dim/vloc", "VERTICAL_DIM_EDGE", _RC)
               call MAPL_InfoSet(info, namespace_ // "/vertical_grid/num_levels", num_levels-1, _RC)
            else if (vert_staggerLoc == VERTICAL_STAGGER_CENTER) then
               call MAPL_InfoSet(info, namespace_ // "/vertical_dim/vloc", "VERTICAL_DIM_CENTER", _RC)
               call MAPL_InfoSet(info, namespace_ // "/vertical_grid/num_levels", num_levels, _RC)
            else
               _FAIL('unsupported vertical stagger')
            end if
         end if

      end if

      if (present(is_active)) then
         call MAPL_InfoSet(info, namespace_ // KEY_IS_ACTIVE, is_active, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_set_internal

   subroutine field_info_get_internal(info, unusable, &
        namespace, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        units, long_name, standard_name, &
        ungridded_dims, &
        vert_only, &
        is_active, &
        rc)

      type(ESMF_Info), intent(in) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      character(:), optional, allocatable, intent(out) :: units
      character(:), optional, allocatable, intent(out) :: long_name
      character(:), optional, allocatable, intent(out) :: standard_name
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      logical, optional, intent(out) :: vert_only
      logical, optional, intent(out) :: is_active
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: num_levels_
      type(ESMF_Info) :: ungridded_info
      character(:), allocatable :: vert_staggerloc_str
      type(VerticalStaggerLoc) :: vert_staggerloc_
      character(:), allocatable :: namespace_

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(ungridded_dims)) then
         ungridded_info = ESMF_InfoCreate(info, namespace_ // KEY_UNGRIDDED_DIMS, _RC)
         ungridded_dims = make_UngriddedDims(ungridded_info, _RC)
      end if

      if (present(num_levels) .or. present(num_vgrid_levels)) then
         call MAPL_InfoGet(info, namespace_ // KEY_NUM_LEVELS, num_levels_, _RC)
         if (present(num_levels)) then
            num_levels = num_levels_
         end if
      end if

      if (present(vert_staggerloc) .or. present(num_vgrid_levels)) then
         call MAPL_InfoGet(info, namespace_ // KEY_VERT_STAGGERLOC, vert_staggerloc_str, _RC)
         vert_staggerloc_ = VerticalStaggerLoc(vert_staggerloc_str)
         if (present(vert_staggerloc)) then
            vert_staggerloc = vert_staggerloc_
         end if
      end if

      if (present(num_vgrid_levels)) then
         if (vert_staggerloc_ == VERTICAL_STAGGER_NONE) then
            num_vgrid_levels = 0
         else if (vert_staggerloc_ == VERTICAL_STAGGER_EDGE) then
            num_vgrid_levels = num_levels_ - 1
         else if (vert_staggerloc_ == VERTICAL_STAGGER_CENTER) then
            num_vgrid_levels = num_levels_
         else
            _FAIL('unsupported vertical stagger')
         end if
      end if

      if (present(units)) then
         call MAPL_InfoGet(info, namespace_ // KEY_UNITS, units, _RC)
      end if

      if (present(long_name)) then
         call MAPL_InfoGet(info, namespace_ // KEY_LONG_NAME, long_name, _RC)
      end if

      if (present(standard_name)) then
         call MAPL_InfoGet(info, namespace_ // KEY_STANDARD_NAME, standard_name, _RC)
      end if

      if (present(vert_only)) then
         call MAPL_InfoGet(info, namespace_ // KEY_VERT_ONLY, vert_only, _RC)
      end if

      if (present(is_active)) then
         call MAPL_InfoGet(info, namespace_ // KEY_IS_ACTIVE, is_active, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_get_internal


   subroutine info_field_get_shared_i4(field, key, value, unusable, rc)
      type(ESMF_Field), intent(in) :: field
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call MAPL_InfoGet(field_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_field_get_shared_i4

   subroutine info_field_get_shared_r4(field, key, value, unusable, rc)
      type(ESMF_Field), intent(in) :: field
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call MAPL_InfoGet(field_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_field_get_shared_r4

   subroutine info_field_set_shared_i4(field, key, value, rc)
      type(ESMF_Field), intent(in) :: field
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call MAPL_InfoSet(field_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_field_set_shared_i4

   subroutine info_field_set_shared_r4(field, key, value, rc)
      type(ESMF_Field), intent(in) :: field
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call MAPL_InfoSet(field_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_field_set_shared_r4

   subroutine field_info_copy_shared(field_in, field_out, rc)
      type(ESMF_Field), intent(in) :: field_in
      type(ESMF_Field), intent(inout) :: field_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: shared_info, info_out

      shared_info = MAPL_InfoCreateFromShared(field_in, _RC)
      call ESMF_InfoGetFromHost(field_out, info_out, _RC)
      ! 'force' may be needed in next, but ideally the import field will not yet have an shared space
      call MAPL_InfoSet(info_out, INFO_SHARED_NAMESPACE, shared_info, _RC)

      _RETURN(_SUCCESS)
   end subroutine field_info_copy_shared
      
   function concat(namespace, key) result(full_key)
      character(*), intent(in) :: namespace
      character(*), intent(in) :: key
      character(len(namespace)+len(key)+1) :: full_key

      if (key(1:1) == '/') then
         full_key = namespace // key
         return
      end if
      full_key = namespace // '/' //key

   end function concat


end module mapl3g_FieldInfo

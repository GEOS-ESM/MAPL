#include "MAPL_Generic.h"

module mapl3g_FieldInfo
   use mapl3g_esmf_info_keys, only: INFO_INTERNAL_NAMESPACE
   use mapl3g_InfoUtilities
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf, only: ESMF_Field
   use esmf, only: ESMF_Info, ESMF_InfoGetFromHost, ESMF_InfoCreate
   implicit none(type,external)
   private

   public :: MAPL_FieldInfoSetInternal
   public :: MAPL_FieldInfoGetInternal

   public :: KEY_UNITS
   public :: KEY_LONG_NAME
   public :: KEY_STANDARD_NAME
   public :: KEY_NUM_LEVELS
   public :: KEY_VERT_STAGGERLOC
   public :: KEY_UNGRIDDED_DIMS

   public :: KEY_UNDEF_VALUE
   public :: KEY_MISSING_VALUE
   public :: KEY_FILL_VALUE

   interface MAPL_FieldInfoSetInternal
      module procedure field_info_set_internal
   end interface MAPL_FieldInfoSetInternal

   interface MAPL_FieldInfoGetInternal
      module procedure field_info_get_internal
   end interface

   character(*), parameter :: KEY_UNITS = "/units"
   character(*), parameter :: KEY_LONG_NAME = "/long_name"
   character(*), parameter :: KEY_STANDARD_NAME = "/standard_name"
   character(*), parameter :: KEY_NUM_LEVELS = "/num_levels"
   character(*), parameter :: KEY_VERT_STAGGERLOC = "/vert_staggerloc"
   character(*), parameter :: KEY_UNGRIDDED_DIMS = "/ungridded_dims"

   character(*), parameter :: KEY_UNDEF_VALUE = "/undef_value"
   character(*), parameter :: KEY_MISSING_VALUE = "/missing_value"
   character(*), parameter :: KEY_FILL_VALUE = "/_FillValue"

contains

   subroutine field_info_set_internal(field, unusable, num_levels, &
        vert_staggerloc, ungridded_dims, &
        units, long_name, standard_name, &
        rc)

      type(ESMF_Field), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      character(*), optional, intent(in) :: standard_name
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_Info) :: ungridded_info, field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)

      if (present(ungridded_dims)) then
         ungridded_info = ungridded_dims%make_info(_RC)
         call MAPL_InfoSet(field_info, INFO_INTERNAL_NAMESPACE // KEY_UNGRIDDED_DIMS, ungridded_info, _RC)
      end if

      if (present(units)) then
         call MAPL_InfoSet(field_info, INFO_INTERNAL_NAMESPACE // KEY_UNITS, units, _RC)
      end if

      if (present(long_name)) then
         call MAPL_InfoSet(field_info, INFO_INTERNAL_NAMESPACE // KEY_LONG_NAME, long_name, _RC)
      end if

      if (present(standard_name)) then
         call MAPL_InfoSet(field_info, INFO_INTERNAL_NAMESPACE // KEY_STANDARD_NAME, standard_name, _RC)
      end if

      if (present(num_levels)) then
         call MAPL_InfoSet(field_info, INFO_INTERNAL_NAMESPACE // KEY_NUM_LEVELS, num_levels, _RC)
      end if

      if (present(vert_staggerloc)) then
         call MAPL_InfoSet(field_info, INFO_INTERNAL_NAMESPACE // KEY_VERT_STAGGERLOC, vert_staggerloc%to_string(), _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_set_internal

   subroutine field_info_get_internal(field, unusable, &
        num_levels, vert_staggerloc, units, long_name, standard_name, &
        ungridded_dims, rc)

      type(ESMF_Field), intent(in) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: num_levels
      integer, optional, intent(out) :: vert_staggerloc
      character(:), optional, allocatable, intent(out) :: units
      character(:), optional, allocatable, intent(out) :: long_name
      character(:), optional, allocatable, intent(out) :: standard_name
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: ungridded_info, field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)

      if (present(ungridded_dims)) then
         ungridded_info = ESMF_InfoCreate(field_info, INFO_INTERNAL_NAMESPACE // KEY_UNGRIDDED_DIMS, _RC)
         ungridded_dims = make_UngriddedDims(ungridded_info, _RC)
      end if

      if (present(units)) then
         call MAPL_InfoGet(field_info, INFO_INTERNAL_NAMESPACE // KEY_UNITS, units, _RC)
      end if

      if (present(long_name)) then
         call MAPL_InfoGet(field_info, INFO_INTERNAL_NAMESPACE // KEY_LONG_NAME, long_name, _RC)
      end if

      if (present(standard_name)) then
         call MAPL_InfoGet(field_info, INFO_INTERNAL_NAMESPACE // KEY_STANDARD_NAME, standard_name, _RC)
      end if

      if (present(num_levels)) then
         call MAPL_InfoGet(field_info, INFO_INTERNAL_NAMESPACE // KEY_NUM_LEVELS, num_levels, _RC)
      end if

      if (present(vert_staggerloc)) then
         call MAPL_InfoGet(field_info, INFO_INTERNAL_NAMESPACE // KEY_VERT_STAGGERLOC, vert_staggerloc, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_get_internal

end module mapl3g_FieldInfo

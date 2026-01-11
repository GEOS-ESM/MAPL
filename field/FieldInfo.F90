#include "MAPL.h"

module mapl3g_FieldInfo
   use mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
   use mapl3g_esmf_info_keys, only: INFO_SHARED_NAMESPACE
   use mapl3g_esmf_info_keys, only: INFO_INTERNAL_NAMESPACE
   use mapl3g_esmf_info_keys, only: INFO_PRIVATE_NAMESPACE
   use mapl3g_InfoUtilities
   use mapl3g_VerticalGrid_API
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl3g_StateItemAllocation
   use mapl3g_RestartModes, only: RestartMode, MAPL_RESTART_REQUIRED
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   use gftl2_StringVector
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
      module procedure field_info_set_internal_restart_mode
   end interface FieldInfoSetInternal

   interface FieldInfoGetInternal
      module procedure field_info_get_internal
      module procedure field_info_get_internal_restart_mode
   end interface FieldInfoGetInternal

   interface FieldInfoCopyShared
      procedure :: field_info_copy_shared
   end interface FieldInfoCopyShared

   character(*), parameter :: KEY_VGRID_ID = "/vgrid_id"
   character(*), parameter :: KEY_TYPEKIND = "/typekind"
   character(*), parameter :: KEY_UNITS = "/units"
   character(*), parameter :: KEY_ATTRIBUTES = "/attributes"
   character(*), parameter :: KEY_LONG_NAME = "/long_name"
   character(*), parameter :: KEY_STANDARD_NAME = "/standard_name"
   character(*), parameter :: KEY_NUM_LEVELS = "/num_levels"
   character(*), parameter :: KEY_NUM_VGRID_LEVELS = "/num_vgrid_levels"
   character(*), parameter :: KEY_VERT_STAGGERLOC = "/vert_staggerloc"
   character(*), parameter :: KEY_VERT_DIM = "/vert_dim"
   character(*), parameter :: KEY_UNGRIDDED_DIMS = "/ungridded_dims"
   character(*), parameter :: KEY_ALLOCATION_STATUS = "/allocation_status"
   character(*), parameter :: KEY_REGRIDDER_PARAM = "/EsmfRegridderParam"

   character(*), parameter :: KEY_UNDEF_VALUE = "/undef_value"
   character(*), parameter :: KEY_MISSING_VALUE = "/missing_value"
   character(*), parameter :: KEY_FILL_VALUE = "/_FillValue"

   character(*), parameter :: KEY_RESTART_MODE = "/restart_mode"
   character(*), parameter :: KEY_HAS_DEFERRED_ASPECTS = "/has_deferred_aspects"

contains

   subroutine field_info_set_internal(info, unusable, &
        namespace, &
        typekind, &
        num_levels, vert_staggerloc, &
        ungridded_dims, &
        units, long_name, standard_name, &
        vgrid_id, &
        allocation_status, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)
      type(ESMF_Info), intent(inout) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      integer, optional, intent(in) :: vgrid_id
      type(esmf_typekind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      character(*), optional, intent(in) :: standard_name
      type(StateItemAllocation), optional, intent(in) :: allocation_status
      logical, optional, intent(in) :: has_deferred_aspects
      type(esmf_info), optional, intent(in) :: regridder_param_info
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: ungridded_info
      character(:), allocatable :: namespace_
      character(:), allocatable :: str

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(vgrid_id)) then
         call mapl_InfoSet(info, namespace_ // KEY_VGRID_ID, vgrid_id, _RC)
      end if

      if (present(typekind)) then
         str = to_string(typekind)
         call MAPL_InfoSet(info, namespace_ // KEY_TYPEKIND, str, _RC)
      end if

      if (present(ungridded_dims)) then
         ungridded_info = ungridded_dims%make_info(_RC)
         call MAPL_InfoSet(info, namespace_ // KEY_UNGRIDDED_DIMS, ungridded_info, _RC)
         call esmf_InfoDestroy(ungridded_info, _RC)
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

      if (present(regridder_param_info)) then
         call MAPL_InfoSet(info, namespace_ // KEY_REGRIDDER_PARAM, regridder_param_info, _RC)
         _HERE
      end if

      if (present(vert_staggerloc)) then
         call MAPL_InfoSet(info, namespace_ // KEY_VERT_STAGGERLOC, vert_staggerloc%to_string(), _RC)

         ! Delete later - needed for transition

         if (present(num_levels) .and. present(vert_staggerloc)) then

            if (vert_staggerLoc == VERTICAL_STAGGER_NONE) then
               call MAPL_InfoSet(info, namespace_ // KEY_VERT_DIM, "VERTICAL_DIM_NONE", _RC)
               call MAPL_InfoSet(info, namespace_ // KEY_NUM_VGRID_LEVELS, 0, _RC)
            else if (vert_staggerLoc == VERTICAL_STAGGER_EDGE) then
               call MAPL_InfoSet(info, namespace_ // KEY_VERT_DIM, "VERTICAL_DIM_EDGE", _RC)
               call MAPL_InfoSet(info, namespace_ // KEY_NUM_VGRID_LEVELS, num_levels-1, _RC)
            else if (vert_staggerLoc == VERTICAL_STAGGER_CENTER) then
               call MAPL_InfoSet(info, namespace_ // KEY_VERT_DIM, "VERTICAL_DIM_CENTER", _RC)
               call MAPL_InfoSet(info, namespace_ // KEY_NUM_VGRID_LEVELS, num_levels, _RC)
            else
               _FAIL('unsupported vertical stagger')
            end if
         end if

      end if

      if (present(allocation_status)) then
         call MAPL_InfoSet(info, namespace_ // KEY_ALLOCATION_STATUS, allocation_status%to_string(), _RC)
      end if

      if (present(has_deferred_aspects)) then
         call MAPL_InfoSet(info, namespace_ // KEY_HAS_DEFERRED_ASPECTS, has_deferred_aspects, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_set_internal

   subroutine field_info_get_internal(info, unusable, &
        namespace, &
        vgrid_id, &
        typekind, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        units, &
        long_name, standard_name, &
        ungridded_dims, &
        allocation_status, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)
      type(ESMF_Info), intent(in) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      integer, optional, intent(out) :: vgrid_id
      type(esmf_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      character(:), optional, allocatable, intent(out) :: units
      character(:), optional, allocatable, intent(out) :: long_name
      character(:), optional, allocatable, intent(out) :: standard_name
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      logical, optional, intent(out) :: has_deferred_aspects
      type(esmf_Info), allocatable, optional, intent(out) :: regridder_param_info
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: num_levels_
      type(esmf_Info) :: ungridded_info
      character(:), allocatable :: vert_staggerloc_str, allocation_status_str
      type(VerticalStaggerLoc) :: vert_staggerloc_
      character(:), allocatable :: namespace_ 
      character(:), allocatable :: str
      logical :: is_present
      
      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(vgrid_id)) then
         call esmf_InfoGet(info, key=namespace_ // KEY_VGRID_ID, &
              value=vgrid_id, default=VERTICAL_GRID_NOT_FOUND, _RC)
      end if

      if (present(typekind)) then
         call mapl_InfoGet(info, namespace_ // KEY_TYPEKIND, str, _RC)
         typekind = to_Typekind(str)
      end if

      if (present(ungridded_dims)) then
         is_present = esmf_InfoIsPresent(info, namespace_ // KEY_UNGRIDDED_DIMS, _RC)
         if (is_present) then
            ungridded_info = ESMF_InfoCreate(info, namespace_ // KEY_UNGRIDDED_DIMS, _RC)
            ungridded_dims = make_UngriddedDims(ungridded_info, _RC)
            call esmf_InfoDestroy(ungridded_info, _RC)
         else
            ungridded_dims = UngriddedDims(is_mirror=.true.)
         end if
      end if

      if (present(regridder_param_info)) then
         is_present = esmf_InfoIsPresent(info, namespace_ // KEY_REGRIDDER_PARAM, _RC)
         if (is_present) then
            regridder_param_info = esmf_InfoCreate(info, namespace_ // KEY_REGRIDDER_PARAM, _RC)
         end if
      end if

      if (present(num_levels) .or. present(num_vgrid_levels)) then
         is_present = esmf_InfoIsPresent(info, namespace_ // KEY_NUM_LEVELS, _RC)
         if (is_present) then
            call MAPL_InfoGet(info, namespace_ // KEY_NUM_LEVELS, num_levels_, _RC)
            if (present(num_levels)) then
               num_levels = num_levels_
            end if
         else
            num_levels = 0
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
            num_vgrid_levels = 0 ! num_levels_ must not be used here
         else if (vert_staggerloc_ == VERTICAL_STAGGER_EDGE) then
            num_vgrid_levels = num_levels_ - 1
         else if (vert_staggerloc_ == VERTICAL_STAGGER_CENTER) then
            num_vgrid_levels = num_levels_
         else
            _FAIL('unsupported vertical stagger')
         end if
      end if

      if (present(units)) then ! leave unallocated unless found
         is_present = esmf_InfoIsPresent(info, key=namespace_ // KEY_UNITS, _RC)
         if (is_present) then 
            call MAPL_InfoGet(info, namespace_ // KEY_UNITS, units, _RC)
         end if
      end if

      if (present(long_name)) then
         call MAPL_InfoGet(info, namespace_ // KEY_LONG_NAME, long_name, _RC)
      end if

      if (present(standard_name)) then
         call MAPL_InfoGet(info, namespace_ // KEY_STANDARD_NAME, standard_name, _RC)
      end if

      if (present(allocation_status)) then
         call MAPL_InfoGet(info, namespace_ // KEY_ALLOCATION_STATUS, allocation_status_str, _RC)
         allocation_status = StateItemAllocation(allocation_status_str)
      end if

      if (present(has_deferred_aspects)) then
         call esmf_InfoGet(info, key=namespace_ // KEY_HAS_DEFERRED_ASPECTS, &
              value=has_deferred_aspects, default=.false., _RC)
      end if

     _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_get_internal


   subroutine field_info_set_internal_restart_mode(info, named_alias_id, restart_mode, rc)
      type(ESMF_Info), intent(inout) :: info
      integer, intent(in) :: named_alias_id
      type(RestartMode), intent(in) :: restart_mode
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: id_str, namespace

      id_str = ESMF_UtilStringInt2String(named_alias_id, _RC)
      ! NOTE: the 'alias' is to keep ESMF_Info from getting confused
      namespace = INFO_INTERNAL_NAMESPACE // "/alias" // trim(id_str)

      call MAPL_InfoSet(info, namespace // KEY_RESTART_MODE, restart_mode%get_mode(), _RC)

      _RETURN(_SUCCESS)
   end subroutine field_info_set_internal_restart_mode

   subroutine field_info_get_internal_restart_mode(info, named_alias_id, restart_mode, rc)
      type(ESMF_Info), intent(in) :: info
      integer, intent(in) :: named_alias_id
      type(RestartMode), intent(out) :: restart_mode
      integer, optional, intent(out) :: rc

      integer :: mode, status
      character(:), allocatable :: id_str, namespace, key
      logical :: key_is_present

      id_str = ESMF_UtilStringInt2String(named_alias_id, _RC)
      ! NOTE: the 'alias' is to keep ESMF_Info from getting confused
      namespace = INFO_INTERNAL_NAMESPACE // "/alias" // trim(id_str)

      restart_mode = MAPL_RESTART_REQUIRED
      key = namespace // KEY_RESTART_MODE
      key_is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      if (key_is_present) then
         call MAPL_InfoGet(info, key, mode, _RC)
         call restart_mode%set_mode(mode)
      end if

      _RETURN(_SUCCESS)
   end subroutine field_info_get_internal_restart_mode


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

   function to_string(typekind) result(s)
      character(:), allocatable :: s
      type(esmf_TypeKind_Flag), intent(in) :: typekind

      if (typekind == ESMF_TYPEKIND_R4) then
         s = "R4"
      else if (typekind == ESMF_TYPEKIND_R8) then
         s = "R8"
      else if (typekind == ESMF_TYPEKIND_I4) then
         s = "I4"
      else if (typekind == ESMF_TYPEKIND_I8) then
         s = "I8"
      else if (typekind == ESMF_TYPEKIND_LOGICAL) then
         s = "LOGICAL"
      else if (typekind == MAPL_TYPEKIND_MIRROR) then
         s = "<MIRROR>"
      else
         s = "NOKIND"
      end if

   end function to_string

   function to_typekind(s) result(typekind)
      type(esmf_TypeKind_Flag) :: typekind
      character(*), intent(in) :: s

      select case (s)
      case ("R4")
         typekind = ESMF_TYPEKIND_R4
      case ("R8")
         typekind = ESMF_TYPEKIND_R8
      case ("I4")
         typekind = ESMF_TYPEKIND_I4
      case ("I8")
         typekind = ESMF_TYPEKIND_I8
      case ("LOGICAL")
         typekind = ESMF_TYPEKIND_LOGICAL
      case ("<MIRROR>")
         typekind = MAPL_TYPEKIND_MIRROR
      case default
         typekind = ESMF_NOKIND
      end select

   end function to_typekind


end module mapl3g_FieldInfo

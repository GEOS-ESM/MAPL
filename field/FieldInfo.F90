#include "MAPL.h"

module mapl3g_FieldInfo
   use mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
   use mapl3g_esmf_info_keys, only: INFO_SHARED_NAMESPACE
   use mapl3g_esmf_info_keys, only: INFO_INTERNAL_NAMESPACE
   use mapl3g_esmf_info_keys, only: INFO_PRIVATE_NAMESPACE
   use mapl3g_VerticalGrid
   use mapl3g_InfoUtilities
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl3g_StateItemAllocation
   use mapl3g_RestartModes, only: MAPL_RESTART_MODE, MAPL_RESTART_REQUIRED
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
   public :: FieldInfoSetPrivate
   public :: FieldInfoGetPrivate
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
   end interface FieldInfoGetInternal

   interface FieldInfoSetPrivate
      module procedure field_info_set_private
   end interface FieldInfoSetPrivate

   interface FieldInfoGetPrivate
      module procedure field_info_get_private
   end interface FieldInfoGetPrivate

   interface FieldInfoCopyShared
      procedure :: field_info_copy_shared
   end interface FieldInfoCopyShared

   character(*), parameter :: KEY_VERTICAL_GRID = "/vertical_grid"
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

   character(*), parameter :: KEY_UNDEF_VALUE = "/undef_value"
   character(*), parameter :: KEY_MISSING_VALUE = "/missing_value"
   character(*), parameter :: KEY_FILL_VALUE = "/_FillValue"

   character(*), parameter :: KEY_SPEC_HANDLE = "/spec_handle"
   character(*), parameter :: KEY_RESTART_MODE = "/restart_mode"

   type :: VGridWrapper
      class(VerticalGrid), pointer :: ptr
   end type VGridWrapper

contains

   subroutine field_info_set_internal(info, unusable, &
        namespace, &
        vertical_grid, &
        typekind, &
        num_levels, vert_staggerloc, &
        ungridded_dims, &
        units, long_name, standard_name, &
        attributes, &
        allocation_status, &
        restart_mode, &
        spec_handle, &
        rc)
      type(ESMF_Info), intent(inout) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      class(VerticalGrid), optional, target, intent(in) :: vertical_grid
      type(esmf_typekind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: units
      type(StringVector), optional, intent(in) :: attributes
      character(*), optional, intent(in) :: long_name
      character(*), optional, intent(in) :: standard_name
      type(StateItemAllocation), optional, intent(in) :: allocation_status
      integer(kind=kind(MAPL_RESTART_MODE)), optional, intent(in) :: restart_mode
      integer, optional, intent(in) :: spec_handle(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: ungridded_info
      character(:), allocatable :: attributes_str(:)
      character(:), allocatable :: namespace_
      character(:), allocatable :: str
      type(VGridWrapper) :: vgrid_wrapper
      integer, allocatable :: encoded_vgrid(:)

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(vertical_grid)) then
         vgrid_wrapper%ptr => vertical_grid
         encoded_vgrid = transfer(vgrid_wrapper, [1])
         call mapl_InfoSet(info, namespace_ // KEY_VERTICAL_GRID, encoded_vgrid, _RC)
      end if

      if (present(typekind)) then
         str = to_string(typekind)
         call MAPL_InfoSet(info, namespace_ // KEY_TYPEKIND, str, _RC)
      end if

      if (present(ungridded_dims)) then
         ungridded_info = ungridded_dims%make_info(_RC)
         call MAPL_InfoSet(info, namespace_ // KEY_UNGRIDDED_DIMS, ungridded_info, _RC)
      end if

      if (present(units)) then
         call MAPL_InfoSet(info, namespace_ // KEY_UNITS, units, _RC)
      end if

      if (present(attributes)) then
         attributes_str = make_attributes_string(attributes)
         call MAPL_InfoSet(info, namespace_ // KEY_ATTRIBUTES, attributes_str, _RC)
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

      if (present(spec_handle)) then
         call MAPL_InfoSet(info, namespace_ // KEY_SPEC_HANDLE, spec_handle, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_set_internal

   subroutine field_info_get_internal(info, unusable, &
        namespace, &
        vertical_grid, &
        typekind, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        units, &
        attributes, &
        long_name, standard_name, &
        ungridded_dims, &
        allocation_status, &
        restart_mode, &
        spec_handle, &
        rc)
      type(ESMF_Info), intent(in) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      class(VerticalGrid), optional, allocatable, intent(out) :: vertical_grid
      type(esmf_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      character(:), optional, allocatable, intent(out) :: units
      type(StringVector), optional, intent(out) ::  attributes
      character(:), optional, allocatable, intent(out) :: long_name
      character(:), optional, allocatable, intent(out) :: standard_name
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      integer(kind=kind(MAPL_RESTART_MODE)), optional, intent(in) :: restart_mode
      integer, optional, allocatable, intent(out) :: spec_handle(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: num_levels_
      type(ESMF_Info) :: ungridded_info
      character(:), allocatable :: vert_staggerloc_str, allocation_status_str
      type(VerticalStaggerLoc) :: vert_staggerloc_
      character(:), allocatable :: namespace_ 
      character(:), allocatable :: str
      character(:), allocatable :: attributes_str(:)
      logical :: key_is_present
      integer, allocatable :: encoded_vgrid(:)
      type(VGridWrapper) :: vgrid_wrapper
      logical :: is_present
      
      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(vertical_grid)) then
         
         is_present= esmf_InfoIsPresent(info, namespace_ // KEY_VERTICAL_GRID, _RC)
         if (is_present) then
            call mapl_InfoGet(info, namespace_ //KEY_VERTICAL_GRID, encoded_vgrid, _RC)
            vgrid_wrapper = transfer(encoded_vgrid, vgrid_wrapper)
            vertical_grid = vgrid_wrapper%ptr
         end if
      end if
      if (present(typekind)) then
         call mapl_InfoGet(info, namespace_ // KEY_TYPEKIND, str, _RC)
         typekind = to_Typekind(str)
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

      if (present(attributes)) then
         call mapl_InfoGet(info, namespace_ // KEY_ATTRIBUTES, attributes_str, _RC)
         attributes = from_string(attributes_str)
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

      if (present(spec_handle)) then
         call MAPL_InfoGet(info, namespace_ // KEY_SPEC_HANDLE, spec_handle, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_get_internal

   subroutine field_info_set_private(info, gridcomp_name, short_name, unusable, restart_mode, rc)
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: gridcomp_name
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer(kind=kind(MAPL_RESTART_MODE)), optional, intent(in) :: restart_mode
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: namespace

      namespace = INFO_PRIVATE_NAMESPACE // "/" // trim(gridcomp_name) // "/" // trim(short_name)

      if (present(restart_mode)) then
         call MAPL_InfoSet(info, namespace // KEY_RESTART_MODE, restart_mode, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_set_private

   subroutine field_info_get_private(info, gridcomp_name, short_name, unusable, restart_mode, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: gridcomp_name
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer(kind=kind(MAPL_RESTART_MODE)), optional, intent(out) :: restart_mode
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: namespace, key
      logical :: key_is_present

      namespace = INFO_PRIVATE_NAMESPACE // "/" // trim(gridcomp_name) // "/" // trim(short_name)

      if (present(restart_mode)) then
         key = namespace // KEY_RESTART_MODE
         key_is_present = ESMF_InfoIsPresent(info, key=key, _RC)
         restart_mode = MAPL_RESTART_REQUIRED
         if (key_is_present) then
            call MAPL_InfoGet(info, key, restart_mode, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_info_get_private

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


   function make_attributes_string(attributes) result(str)
      character(:), allocatable :: str(:)
      type(StringVector), intent(in) :: attributes

      integer :: status
      integer :: i, n, maxlen

      maxlen = 0
      n = attributes%size()
      do i = 1, n
         maxlen = max(maxlen, len(attributes%of(i)))
      end do

      allocate(character(len=maxlen) :: str(n))
      do i = 1, n
         str(i) = attributes%of(i)
      end do

   end function make_attributes_string

   function from_string(str) result(attributes)
      type(StringVector) :: attributes
      character(*), intent(in) :: str(:)

      integer :: i, n

      n = size(str)
      do i = 1, n
         call attributes%push_back(trim(str(i)))
      end do

   end function from_string
      
end module mapl3g_FieldInfo

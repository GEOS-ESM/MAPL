#include "MAPL.h"

module mapl3g_FieldInfo

   use mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
   use mapl3g_esmf_info_keys, only: INFO_SHARED_NAMESPACE
   use mapl3g_esmf_info_keys, only: INFO_INTERNAL_NAMESPACE
   use mapl3g_esmf_info_keys, only: INFO_PRIVATE_NAMESPACE
   use mapl3g_InfoUtilities
   use mapl3g_VerticalGrid_API
   use mapl3g_UngriddedDims
   use mapl3g_QuantityTypeMetadata
   use mapl3g_NormalizationMetadata
   use mapl3g_ConservationMetadata
   use mapl3g_VerticalStaggerLoc
   use mapl3g_VerticalAlignment
   use mapl3g_StateItemAllocation
   use mapl3g_RestartModes, only: RestartMode, MAPL_RESTART_REQUIRED
   use mapl3g_HorizontalDimsSpec, only: HorizontalDimsSpec, HORIZONTAL_DIMS_UNKNOWN, to_HorizontalDimsSpec
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

   character(*), parameter :: KEY_TYPEKIND = "/typekind"
   character(*), parameter :: KEY_UNITS = "/units"
   character(*), parameter :: KEY_ATTRIBUTES = "/attributes"
   character(*), parameter :: KEY_LONG_NAME = "/long_name"
   character(*), parameter :: KEY_STANDARD_NAME = "/standard_name"
   character(*), parameter :: KEY_HORIZONTAL_DIMS_SPEC = "/horizontal_dims_spec"
   character(*), parameter :: KEY_VGRID_ID = "/vgrid_id"
   character(*), parameter :: KEY_VERT_STAGGERLOC = "/vert_staggerloc"
   character(*), parameter :: KEY_VERT_ALIGNMENT = "/vert_alignment"
   character(*), parameter :: KEY_VERT_DIM = "/vert_dim"
   character(*), parameter :: KEY_UNGRIDDED_DIMS = "/ungridded_dims"
   character(*), parameter :: KEY_ALLOCATION_STATUS = "/allocation_status"
   character(*), parameter :: KEY_QUANTITY_TYPE_METADATA = "/quantity_type_metadata"
   character(*), parameter :: KEY_NORMALIZATION_METADATA = "/normalization_metadata"
   character(*), parameter :: KEY_CONSERVATION_METADATA = "/conservation_metadata"
   character(*), parameter :: KEY_REGRIDDER_PARAM = "/EsmfRegridderParam"

   character(*), parameter :: KEY_UNDEF_VALUE = "/undef_value"
   character(*), parameter :: KEY_MISSING_VALUE = "/missing_value"
   character(*), parameter :: KEY_FILL_VALUE = "/_FillValue"

   character(*), parameter :: KEY_RESTART_MODE = "/restart_mode"
   character(*), parameter :: KEY_HAS_DEFERRED_ASPECTS = "/has_deferred_aspects"
   character(len=*), parameter :: DELIMITER = '/'

contains

   subroutine field_info_set_internal(info, unusable, &
        namespace, &
        typekind, &
        horizontal_dims_spec, &
        vgrid_id, vert_staggerloc, vert_alignment, &
        ungridded_dims, &
        quantity_type_metadata, &
        normalization_metadata, &
        conservation_metadata, &
        units, long_name, standard_name, &
        allocation_status, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)
      type(ESMF_Info), intent(inout) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      type(esmf_typekind_Flag), optional, intent(in) :: typekind
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec
      integer, optional, intent(in) :: vgrid_id
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(VerticalAlignment), optional, intent(in) :: vert_alignment
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      type(QuantityTypeMetadata), optional, intent(in) :: quantity_type_metadata
      type(NormalizationMetadata), optional, intent(in) :: normalization_metadata
      type(ConservationMetadata), optional, intent(in) :: conservation_metadata
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      character(*), optional, intent(in) :: standard_name
      type(StateItemAllocation), optional, intent(in) :: allocation_status
      logical, optional, intent(in) :: has_deferred_aspects
      type(esmf_info), optional, intent(in) :: regridder_param_info
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: ungridded_info, quantity_info, normalization_info, conservation_info
      character(:), allocatable :: namespace_
      character(:), allocatable :: str
      logical :: isPresent

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(typekind)) then
         str = to_string(typekind)
         call MAPL_InfoSet(info, namespace_ // KEY_TYPEKIND, str, _RC)
      end if

      if (present(horizontal_dims_spec)) then
         str = horizontal_dims_spec%to_string()
         call MAPL_InfoSet(info, namespace_ // KEY_HORIZONTAL_DIMS_SPEC, str, _RC)
      end if

      if (present(vgrid_id)) then
         call mapl_InfoSet(info, namespace_ // KEY_VGRID_ID, vgrid_id, _RC)
      end if

      if (present(ungridded_dims)) then
         ungridded_info = ungridded_dims%make_info(_RC)
         call MAPL_InfoSet(info, namespace_ // KEY_UNGRIDDED_DIMS, ungridded_info, _RC)
         call esmf_InfoDestroy(ungridded_info, _RC)
      end if

      if (present(quantity_type_metadata)) then
         quantity_info = quantity_type_metadata%make_info(_RC)
         call MAPL_InfoSet(info, namespace_ // KEY_QUANTITY_TYPE_METADATA, quantity_info, _RC)
         call esmf_InfoDestroy(quantity_info, _RC)
      end if

      if (present(normalization_metadata)) then
         normalization_info = normalization_metadata%make_info(_RC)
         call MAPL_InfoSet(info, namespace_ // KEY_NORMALIZATION_METADATA, normalization_info, _RC)
         call esmf_InfoDestroy(normalization_info, _RC)
      end if

      if (present(conservation_metadata)) then
         conservation_info = conservation_metadata%make_info(_RC)
         call MAPL_InfoSet(info, namespace_ // KEY_CONSERVATION_METADATA, conservation_info, _RC)
         call esmf_InfoDestroy(conservation_info, _RC)
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

      if (present(regridder_param_info)) then
         call MAPL_InfoSet(info, namespace_ // KEY_REGRIDDER_PARAM, regridder_param_info, _RC)
      end if

      if (present(vert_staggerloc)) then
         call MAPL_InfoSet(info, namespace_ // KEY_VERT_STAGGERLOC, vert_staggerloc%to_string(), _RC)
      end if

      if (present(vert_alignment)) then
         call MAPL_InfoSet(info, namespace_ // KEY_VERT_ALIGNMENT, vert_alignment%to_string(), _RC)
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
        typekind, &
        horizontal_dims_spec, &
        vgrid_id, num_levels, num_layers, vert_staggerloc, vert_alignment, num_vgrid_levels, &
        units, &
        long_name, standard_name, &
        ungridded_dims, &
        quantity_type_metadata, &
        normalization_metadata, &
        conservation_metadata, &
        allocation_status, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)
      type(ESMF_Info), intent(in) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
       type(esmf_TypeKind_Flag), optional, intent(out) :: typekind
       type(HorizontalDimsSpec), optional, intent(out) :: horizontal_dims_spec
       integer, optional, intent(out) :: vgrid_id
       integer, optional, intent(out) :: num_levels
       integer, optional, intent(out) :: num_layers
       type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
       type(VerticalAlignment), optional, intent(out) :: vert_alignment
       integer, optional, intent(out) :: num_vgrid_levels
       character(:), optional, allocatable, intent(out) :: units
      character(:), optional, allocatable, intent(out) :: long_name
      character(:), optional, allocatable, intent(out) :: standard_name
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      type(QuantityTypeMetadata), optional, intent(out) :: quantity_type_metadata
      type(NormalizationMetadata), optional, intent(out) :: normalization_metadata
      type(ConservationMetadata), optional, intent(out) :: conservation_metadata
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      logical, optional, intent(out) :: has_deferred_aspects
      type(esmf_Info), allocatable, optional, intent(out) :: regridder_param_info
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: num_levels_
      type(esmf_Info) :: ungridded_info, quantity_info, normalization_info, conservation_info
      character(:), allocatable :: vert_staggerloc_str, vert_alignment_str, allocation_status_str
      type(VerticalStaggerLoc) :: vert_staggerloc_
      character(:), allocatable :: namespace_
      character(:), allocatable :: str
      logical :: is_present

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(typekind)) then
         call mapl_InfoGet(info, namespace_ // KEY_TYPEKIND, str, _RC)
         typekind = to_Typekind(str)
      end if

      if (present(horizontal_dims_spec)) then
         call MAPL_InfoGet(info, namespace_ // KEY_HORIZONTAL_DIMS_SPEC, str, _RC)
         horizontal_dims_spec = to_HorizontalDimsSpec(str)
      end if

      if (present(vgrid_id)) then
         call esmf_InfoGet(info, key=namespace_ // KEY_VGRID_ID, &
              value=vgrid_id, default=VERTICAL_GRID_NOT_FOUND, _RC)
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

      if (present(quantity_type_metadata)) then
         is_present = ESMF_InfoIsPresent(info, namespace_ // KEY_QUANTITY_TYPE_METADATA, _RC)
         if (is_present) then
            quantity_info = ESMF_InfoCreate(info, namespace_ // KEY_QUANTITY_TYPE_METADATA, _RC)
            quantity_type_metadata = make_QuantityTypeMetadata(quantity_info, _RC)
            call ESMF_InfoDestroy(quantity_info, _RC)
         else
            quantity_type_metadata = QuantityTypeMetadata()  ! mirror
         end if
      end if

      if (present(normalization_metadata)) then
         is_present = ESMF_InfoIsPresent(info, namespace_ // KEY_NORMALIZATION_METADATA, _RC)
         if (is_present) then
            normalization_info = ESMF_InfoCreate(info, namespace_ // KEY_NORMALIZATION_METADATA, _RC)
            normalization_metadata = make_NormalizationMetadata(normalization_info, _RC)
            call ESMF_InfoDestroy(normalization_info, _RC)
         else
            normalization_metadata = NormalizationMetadata()  ! mirror
         end if
      end if

      if (present(conservation_metadata)) then
         is_present = ESMF_InfoIsPresent(info, namespace_ // KEY_CONSERVATION_METADATA, _RC)
         if (is_present) then
            conservation_info = ESMF_InfoCreate(info, namespace_ // KEY_CONSERVATION_METADATA, _RC)
            conservation_metadata = make_ConservationMetadata(conservation_info, _RC)
            call ESMF_InfoDestroy(conservation_info, _RC)
         else
            conservation_metadata = ConservationMetadata()  ! mirror
         end if
      end if

      if (present(regridder_param_info)) then
         is_present = esmf_InfoIsPresent(info, namespace_ // KEY_REGRIDDER_PARAM, _RC)
         if (is_present) then
            regridder_param_info = esmf_InfoCreate(info, namespace_ // KEY_REGRIDDER_PARAM, _RC)
         end if
      end if

       ! Derive num_levels from vgrid_id + vert_staggerloc
       if (present(num_levels) .or. present(num_layers) .or. present(num_vgrid_levels)) then
          call derive_num_levels_from_vgrid(info, namespace_, num_levels, num_layers, num_vgrid_levels, _RC)
       end if

      if (present(vert_staggerloc) .and. .not. present(num_levels) .and. .not. present(num_vgrid_levels)) then
         call MAPL_InfoGet(info, namespace_ // KEY_VERT_STAGGERLOC, vert_staggerloc_str, _RC)
         vert_staggerloc = VerticalStaggerLoc(vert_staggerloc_str)
      end if

      if (present(vert_alignment)) then
         is_present = esmf_InfoIsPresent(info, namespace_ // KEY_VERT_ALIGNMENT, _RC)
         if (is_present) then
            call MAPL_InfoGet(info, namespace_ // KEY_VERT_ALIGNMENT, vert_alignment_str, _RC)
            vert_alignment = VerticalAlignment(vert_alignment_str)
         else
            vert_alignment = VALIGN_WITH_GRID  ! Default value
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

      if (key(1:1) == DELIMITER) then
         full_key = namespace // key
         return
      end if
      full_key = namespace // DELIMITER //key

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

     subroutine derive_num_levels_from_vgrid(info, namespace, num_levels, num_layers, num_vgrid_levels, rc)
        type(ESMF_Info), intent(in) :: info
        character(*), intent(in) :: namespace
        integer, optional, intent(out) :: num_levels
        integer, optional, intent(out) :: num_layers
        integer, optional, intent(out) :: num_vgrid_levels
        integer, optional, intent(out) :: rc

      integer :: status
      integer :: vgrid_id_local
      integer :: num_vgrid_levels_local
      integer :: num_levels_local
      type(VerticalGridManager), pointer :: vgrid_manager
      class(VerticalGrid), pointer :: vgrid_ptr
      character(:), allocatable :: vert_staggerloc_str
      type(VerticalStaggerLoc) :: vert_staggerloc_local

      ! Get vgrid_id
      call esmf_InfoGet(info, namespace // KEY_VGRID_ID, &
           vgrid_id_local, default=VERTICAL_GRID_NOT_FOUND, _RC)

       ! Early return for no vertical grid
       if (vgrid_id_local == VERTICAL_GRID_NOT_FOUND) then
          if (present(num_levels)) num_levels = 0
          if (present(num_layers)) num_layers = 0
          if (present(num_vgrid_levels)) num_vgrid_levels = 0
          _RETURN(_SUCCESS)
       end if

      ! Get vert_staggerloc
      call MAPL_InfoGet(info, namespace // KEY_VERT_STAGGERLOC, vert_staggerloc_str, _RC)
      vert_staggerloc_local = VerticalStaggerLoc(vert_staggerloc_str)

      ! Derive num_levels from vgrid
      vgrid_manager => get_vertical_grid_manager()
      vgrid_ptr => vgrid_manager%get_grid(id=vgrid_id_local, _RC)
      num_vgrid_levels_local = vgrid_ptr%get_num_layers()
      num_levels_local = vert_staggerloc_local%get_num_levels(num_vgrid_levels_local)

       ! Set output values
       if (present(num_levels)) num_levels = num_levels_local
       if (present(num_layers)) num_layers = num_vgrid_levels_local
       if (present(num_vgrid_levels)) num_vgrid_levels = num_vgrid_levels_local

      _RETURN(_SUCCESS)
   end subroutine derive_num_levels_from_vgrid

end module mapl3g_FieldInfo

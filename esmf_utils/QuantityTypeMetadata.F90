#include "MAPL.h"

module mapl3g_QuantityTypeMetadata
   use mapl3g_QuantityType
   use mapl3g_NormalizationType
   use mapl3g_InfoUtilities
   use mapl_ErrorHandling
   use mapl_Constants, only: MAPL_UNDEF => MAPL_UNDEFINED_REAL
   use esmf
   
   implicit none(type, external)
   private
   
   public :: QuantityTypeMetadata
   public :: make_QuantityTypeMetadata
   public :: operator(==)
   public :: operator(/=)
   
   type :: QuantityTypeMetadata
      private
      logical :: is_mirror_ = .false.
      type(QuantityType) :: quantity_type = QUANTITY_UNKNOWN
      character(:), allocatable :: dimensions
      type(MixingRatioBasis) :: basis = BASIS_NONE
      real, allocatable :: molecular_weight
      ! Derived properties
      logical :: conservative_regridable = .false.
      type(NormalizationType) :: normalization_type = NORMALIZE_NONE
      real :: normalization_scale = 1.0
      character(:), allocatable :: aux_field_name
   contains
      procedure :: make_info
      procedure :: is_mirror
      procedure :: get_quantity_type
      procedure :: get_dimensions
      procedure :: get_basis
      procedure :: get_molecular_weight
      procedure :: has_molecular_weight
      procedure :: get_conservative_regridable
      procedure :: get_normalization_type
      procedure :: get_normalization_scale
      procedure :: get_aux_field_name
   end type QuantityTypeMetadata
   
   interface QuantityTypeMetadata
      module procedure new_QuantityTypeMetadata
      module procedure new_QuantityTypeMetadata_mirror
   end interface QuantityTypeMetadata
   
   interface operator(==)
      module procedure equal_to
   end interface operator(==)
   
   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)
   
   ! Info keys
   character(*), parameter :: KEY_IS_MIRROR = "/is_mirror"
   character(*), parameter :: KEY_QUANTITY_TYPE = "/quantity_type"
   character(*), parameter :: KEY_DIMENSIONS = "/dimensions"
   character(*), parameter :: KEY_MIXING_RATIO_BASIS = "/mixing_ratio_basis"
   character(*), parameter :: KEY_MOLECULAR_WEIGHT = "/molecular_weight"
   character(*), parameter :: KEY_CONSERVATIVE_REGRIDABLE = "/conservative_regridable"
   character(*), parameter :: KEY_NORMALIZATION_TYPE = "/normalization_type"
   character(*), parameter :: KEY_NORMALIZATION_SCALE = "/normalization_scale"
   character(*), parameter :: KEY_AUX_FIELD_NAME = "/aux_field_name"
   
contains

   function new_QuantityTypeMetadata(quantity_type, unusable, &
        dimensions, basis, molecular_weight, &
        conservative_regridable, normalization_type, normalization_scale, aux_field_name) &
        result(metadata)
      type(QuantityTypeMetadata) :: metadata
      type(QuantityType), intent(in) :: quantity_type
      class(*), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: dimensions
      type(MixingRatioBasis), optional, intent(in) :: basis
      real, optional, intent(in) :: molecular_weight
      logical, optional, intent(in) :: conservative_regridable
      type(NormalizationType), optional, intent(in) :: normalization_type
      real, optional, intent(in) :: normalization_scale
      character(*), optional, intent(in) :: aux_field_name
      
      metadata%is_mirror_ = .false.
      metadata%quantity_type = quantity_type
      
      if (present(dimensions)) metadata%dimensions = dimensions
      if (present(basis)) metadata%basis = basis
      if (present(molecular_weight)) then
         allocate(metadata%molecular_weight)
         metadata%molecular_weight = molecular_weight
      end if
      if (present(conservative_regridable)) metadata%conservative_regridable = conservative_regridable
      if (present(normalization_type)) metadata%normalization_type = normalization_type
      if (present(normalization_scale)) metadata%normalization_scale = normalization_scale
      if (present(aux_field_name)) metadata%aux_field_name = aux_field_name
      
      _UNUSED_DUMMY(unusable)
   end function new_QuantityTypeMetadata
   
   function new_QuantityTypeMetadata_mirror() result(metadata)
      type(QuantityTypeMetadata) :: metadata
      metadata%is_mirror_ = .true.
   end function new_QuantityTypeMetadata_mirror
   
   logical function is_mirror(this)
      class(QuantityTypeMetadata), intent(in) :: this
      is_mirror = this%is_mirror_
   end function is_mirror
   
   function get_quantity_type(this) result(qtype)
      type(QuantityType) :: qtype
      class(QuantityTypeMetadata), intent(in) :: this
      qtype = this%quantity_type
   end function get_quantity_type
   
   function get_dimensions(this) result(dims)
      character(:), allocatable :: dims
      class(QuantityTypeMetadata), intent(in) :: this
      if (allocated(this%dimensions)) then
         dims = this%dimensions
      else
         dims = ''  ! Return empty string if not allocated
      end if
   end function get_dimensions
   
   function get_basis(this) result(basis)
      type(MixingRatioBasis) :: basis
      class(QuantityTypeMetadata), intent(in) :: this
      basis = this%basis
   end function get_basis
   
   real function get_molecular_weight(this)
      class(QuantityTypeMetadata), intent(in) :: this
      if (allocated(this%molecular_weight)) then
         get_molecular_weight = this%molecular_weight
      else
         get_molecular_weight = MAPL_UNDEF
      end if
   end function get_molecular_weight
   
   logical function has_molecular_weight(this)
      class(QuantityTypeMetadata), intent(in) :: this
      has_molecular_weight = allocated(this%molecular_weight)
   end function has_molecular_weight
   
   logical function get_conservative_regridable(this)
      class(QuantityTypeMetadata), intent(in) :: this
      get_conservative_regridable = this%conservative_regridable
   end function get_conservative_regridable
   
   function get_normalization_type(this) result(ntype)
      type(NormalizationType) :: ntype
      class(QuantityTypeMetadata), intent(in) :: this
      ntype = this%normalization_type
   end function get_normalization_type
   
   real function get_normalization_scale(this)
      class(QuantityTypeMetadata), intent(in) :: this
      get_normalization_scale = this%normalization_scale
   end function get_normalization_scale
   
   function get_aux_field_name(this) result(name)
      character(:), allocatable :: name
      class(QuantityTypeMetadata), intent(in) :: this
      if (allocated(this%aux_field_name)) then
         name = this%aux_field_name
      else
         name = ''  ! Return empty string if not allocated
      end if
   end function get_aux_field_name
   
   function make_info(this, rc) result(info)
      type(ESMF_Info) :: info
      class(QuantityTypeMetadata), intent(in) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      
      info = ESMF_InfoCreate(_RC)
      
      call MAPL_InfoSet(info, key=KEY_IS_MIRROR, value=this%is_mirror(), _RC)
      _RETURN_IF(this%is_mirror())
      
      ! Write quantity_type
      call MAPL_InfoSet(info, KEY_QUANTITY_TYPE, this%quantity_type%to_string(), _RC)
      
      ! Write dimensions (optional)
      if (allocated(this%dimensions)) then
         call MAPL_InfoSet(info, KEY_DIMENSIONS, this%dimensions, _RC)
      end if
      
      ! Write basis
      call MAPL_InfoSet(info, KEY_MIXING_RATIO_BASIS, this%basis%to_string(), _RC)
      
      ! Write molecular_weight (optional)
      if (allocated(this%molecular_weight)) then
         call MAPL_InfoSet(info, KEY_MOLECULAR_WEIGHT, this%molecular_weight, _RC)
      end if
      
      ! Write derived properties
      call MAPL_InfoSet(info, KEY_CONSERVATIVE_REGRIDABLE, this%conservative_regridable, _RC)
      call MAPL_InfoSet(info, KEY_NORMALIZATION_TYPE, this%normalization_type%to_string(), _RC)
      call MAPL_InfoSet(info, KEY_NORMALIZATION_SCALE, this%normalization_scale, _RC)
      if (allocated(this%aux_field_name)) then
         call MAPL_InfoSet(info, KEY_AUX_FIELD_NAME, this%aux_field_name, _RC)
      end if
      
      _RETURN(_SUCCESS)
   end function make_info
   
   function make_QuantityTypeMetadata(info, key, rc) result(metadata)
      type(QuantityTypeMetadata) :: metadata
      type(ESMF_Info), intent(in) :: info
      character(*), optional, intent(in) :: key
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: full_key, str_value
      character(:), allocatable :: dims_buffer, aux_field_buffer
      logical, allocatable :: is_mirror_alloc, conservative_regridable_alloc
      logical :: is_mirror, is_present
      real, allocatable :: mw_alloc, normalization_scale_alloc
      
      ! Check for mirror
      is_mirror = .FALSE.
      full_key = KEY_IS_MIRROR
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, key=full_key, value=is_mirror_alloc, _RC)
         is_mirror = is_mirror_alloc
      end if
      
      if (is_mirror) then
         metadata = QuantityTypeMetadata() ! mirror constructor
         _RETURN(_SUCCESS)
      end if
      
      ! Read quantity_type
      full_key = KEY_QUANTITY_TYPE
      if (present(key)) full_key = key // full_key
      call MAPL_InfoGet(info, full_key, str_value, _RC)
      metadata%quantity_type = QuantityType(str_value)
      
      ! Read dimensions (optional)
      full_key = KEY_DIMENSIONS
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, full_key, dims_buffer, _RC)
         metadata%dimensions = dims_buffer
      end if
      
      ! Read basis
      full_key = KEY_MIXING_RATIO_BASIS
      if (present(key)) full_key = key // full_key
      call MAPL_InfoGet(info, full_key, str_value, _RC)
      metadata%basis = MixingRatioBasis(str_value)
      
      ! Read molecular_weight (optional)
      full_key = KEY_MOLECULAR_WEIGHT
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, full_key, mw_alloc, _RC)
         allocate(metadata%molecular_weight)
         metadata%molecular_weight = mw_alloc
      end if
      
      ! Read derived properties
      full_key = KEY_CONSERVATIVE_REGRIDABLE
      if (present(key)) full_key = key // full_key
      call MAPL_InfoGet(info, full_key, conservative_regridable_alloc, _RC)
      metadata%conservative_regridable = conservative_regridable_alloc
      
      full_key = KEY_NORMALIZATION_TYPE
      if (present(key)) full_key = key // full_key
      call MAPL_InfoGet(info, full_key, str_value, _RC)
      metadata%normalization_type = NormalizationType(str_value)
      
      full_key = KEY_NORMALIZATION_SCALE
      if (present(key)) full_key = key // full_key
      call MAPL_InfoGet(info, full_key, normalization_scale_alloc, _RC)
      metadata%normalization_scale = normalization_scale_alloc
      
      full_key = KEY_AUX_FIELD_NAME
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, full_key, aux_field_buffer, _RC)
         metadata%aux_field_name = aux_field_buffer
      end if
      
      _RETURN(_SUCCESS)
   end function make_QuantityTypeMetadata
   
   logical function equal_to(a, b)
      type(QuantityTypeMetadata), intent(in) :: a, b
      
      equal_to = .false.
      
      if (a%is_mirror() .neqv. b%is_mirror()) return
      if (a%is_mirror()) then
         equal_to = .true.
         return
      end if
      
      if (a%quantity_type /= b%quantity_type) return
      if (a%basis /= b%basis) return
      if (allocated(a%dimensions) .neqv. allocated(b%dimensions)) return
      if (allocated(a%dimensions)) then
         if (a%dimensions /= b%dimensions) return
      end if
      if (allocated(a%molecular_weight) .neqv. allocated(b%molecular_weight)) return
      if (allocated(a%molecular_weight)) then
         if (abs(a%molecular_weight - b%molecular_weight) > epsilon(1.0)) return
      end if
      if (a%conservative_regridable .neqv. b%conservative_regridable) return
      if (a%normalization_type /= b%normalization_type) return
      if (abs(a%normalization_scale - b%normalization_scale) > epsilon(1.0)) return
      if (allocated(a%aux_field_name) .neqv. allocated(b%aux_field_name)) return
      if (allocated(a%aux_field_name)) then
         if (a%aux_field_name /= b%aux_field_name) return
      end if
      
      equal_to = .true.
   end function equal_to
   
   logical function not_equal_to(a, b)
      type(QuantityTypeMetadata), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to
   
end module mapl3g_QuantityTypeMetadata

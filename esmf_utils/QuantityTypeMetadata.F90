#include "MAPL.h"

module mapl3g_QuantityTypeMetadata
   use mapl3g_QuantityType
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
   contains
      procedure :: make_info
      procedure :: is_mirror
      procedure :: get_quantity_type
      procedure :: get_dimensions
      procedure :: get_basis
      procedure :: get_molecular_weight
      procedure :: has_molecular_weight
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
   
contains

   function new_QuantityTypeMetadata(quantity_type, unusable, &
        dimensions, basis, molecular_weight) &
        result(metadata)
      type(QuantityTypeMetadata) :: metadata
      type(QuantityType), intent(in) :: quantity_type
      class(*), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: dimensions
      type(MixingRatioBasis), optional, intent(in) :: basis
      real, optional, intent(in) :: molecular_weight
      
      metadata%is_mirror_ = .false.
      metadata%quantity_type = quantity_type
      
      if (present(dimensions)) metadata%dimensions = dimensions
      if (present(basis)) metadata%basis = basis
      if (present(molecular_weight)) then
         allocate(metadata%molecular_weight)
         metadata%molecular_weight = molecular_weight
      end if
      
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
      
      _RETURN(_SUCCESS)
   end function make_info
   
   function make_QuantityTypeMetadata(info, key, rc) result(metadata)
      type(QuantityTypeMetadata) :: metadata
      type(ESMF_Info), intent(in) :: info
      character(*), optional, intent(in) :: key
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: full_key, str_value
      character(:), allocatable :: dims_buffer
      logical :: is_mirror, is_present
      real :: mw_buffer
      
      ! Check for mirror
      is_mirror = .FALSE.
      full_key = KEY_IS_MIRROR
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, key=full_key, value=is_mirror, _RC)
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
         call MAPL_InfoGet(info, full_key, mw_buffer, _RC)
         allocate(metadata%molecular_weight)
         metadata%molecular_weight = mw_buffer
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
      
      equal_to = .true.
   end function equal_to
   
   logical function not_equal_to(a, b)
      type(QuantityTypeMetadata), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to
   
end module mapl3g_QuantityTypeMetadata

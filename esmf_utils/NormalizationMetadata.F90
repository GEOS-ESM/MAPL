#include "MAPL.h"

module mapl3g_NormalizationMetadata
   use mapl3g_NormalizationType
   use mapl3g_InfoUtilities
   use mapl_ErrorHandling
   use esmf
   
   implicit none(type, external)
   private
   
   public :: NormalizationMetadata
   public :: make_NormalizationMetadata
   public :: operator(==)
   public :: operator(/=)
   
   type :: NormalizationMetadata
      private
      logical :: is_mirror_ = .false.
      type(NormalizationType) :: normalization_type = NORMALIZE_NONE
      real :: normalization_scale = 1.0
   contains
      procedure :: make_info
      procedure :: is_mirror
      procedure :: get_normalization_type
      procedure :: get_normalization_scale
   end type NormalizationMetadata
   
   interface NormalizationMetadata
      module procedure new_NormalizationMetadata
      module procedure new_NormalizationMetadata_mirror
   end interface NormalizationMetadata
   
   interface operator(==)
      module procedure equal_to
   end interface operator(==)
   
   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)
   
   ! Info keys
   character(*), parameter :: KEY_IS_MIRROR = "/is_mirror"
   character(*), parameter :: KEY_NORMALIZATION_TYPE = "/normalization_type"
   character(*), parameter :: KEY_NORMALIZATION_SCALE = "/normalization_scale"
   
contains

   function new_NormalizationMetadata(normalization_type, &
        normalization_scale, unusable) result(metadata)
      type(NormalizationMetadata) :: metadata
      type(NormalizationType), intent(in) :: normalization_type
      real, optional, intent(in) :: normalization_scale
      class(*), optional, intent(in) :: unusable
      
      metadata%is_mirror_ = .false.
      
      metadata%normalization_type = normalization_type
      if (present(normalization_scale)) metadata%normalization_scale = normalization_scale
      
      _UNUSED_DUMMY(unusable)
   end function new_NormalizationMetadata
   
   function new_NormalizationMetadata_mirror() result(metadata)
      type(NormalizationMetadata) :: metadata
      metadata%is_mirror_ = .true.
   end function new_NormalizationMetadata_mirror
   
   logical function is_mirror(this)
      class(NormalizationMetadata), intent(in) :: this
      is_mirror = this%is_mirror_
   end function is_mirror
   
   function get_normalization_type(this) result(ntype)
      type(NormalizationType) :: ntype
      class(NormalizationMetadata), intent(in) :: this
      ntype = this%normalization_type
   end function get_normalization_type
   
   real function get_normalization_scale(this)
      class(NormalizationMetadata), intent(in) :: this
      get_normalization_scale = this%normalization_scale
   end function get_normalization_scale
   
   function make_info(this, rc) result(info)
      type(ESMF_Info) :: info
      class(NormalizationMetadata), intent(in) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      
      info = ESMF_InfoCreate(_RC)
      
      call MAPL_InfoSet(info, key=KEY_IS_MIRROR, value=this%is_mirror(), _RC)
      _RETURN_IF(this%is_mirror())
      
      ! Write normalization properties
      call MAPL_InfoSet(info, KEY_NORMALIZATION_TYPE, this%normalization_type%to_string(), _RC)
      call MAPL_InfoSet(info, KEY_NORMALIZATION_SCALE, this%normalization_scale, _RC)
      
      _RETURN(_SUCCESS)
   end function make_info
   
   function make_NormalizationMetadata(info, key, rc) result(metadata)
      type(NormalizationMetadata) :: metadata
      type(ESMF_Info), intent(in) :: info
      character(*), optional, intent(in) :: key
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: full_key, str_value
      logical :: is_mirror, is_present
      real :: normalization_scale_buffer
      
      ! Check for mirror
      is_mirror = .FALSE.
      full_key = KEY_IS_MIRROR
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, key=full_key, value=is_mirror, _RC)
      end if
      
      if (is_mirror) then
         metadata = NormalizationMetadata() ! mirror constructor
         _RETURN(_SUCCESS)
      end if
      
      ! Read normalization_type
      full_key = KEY_NORMALIZATION_TYPE
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, full_key, str_value, _RC)
         metadata%normalization_type = NormalizationType(str_value)
      end if
      
      ! Read normalization_scale
      full_key = KEY_NORMALIZATION_SCALE
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, full_key, normalization_scale_buffer, _RC)
         metadata%normalization_scale = normalization_scale_buffer
      end if
      
      _RETURN(_SUCCESS)
   end function make_NormalizationMetadata
   
   logical function equal_to(lhs, rhs) result(equals)
      type(NormalizationMetadata), intent(in) :: lhs
      type(NormalizationMetadata), intent(in) :: rhs
      
      equals = .false.
      
      if (lhs%is_mirror() .neqv. rhs%is_mirror()) return
      if (lhs%is_mirror()) then
         equals = .true.
         return
      end if
      
      if (lhs%normalization_type /= rhs%normalization_type) return
      if (lhs%normalization_scale /= rhs%normalization_scale) return
      
      equals = .true.
   end function equal_to
   
   logical function not_equal_to(lhs, rhs) result(not_equals)
      type(NormalizationMetadata), intent(in) :: lhs
      type(NormalizationMetadata), intent(in) :: rhs
      not_equals = .not. (lhs == rhs)
   end function not_equal_to

end module mapl3g_NormalizationMetadata

#include "MAPL.h"

module mapl3g_ConservationMetadata
   use mapl3g_ConservationType
   use mapl3g_InfoUtilities
   use mapl_ErrorHandling
   use esmf
   
   implicit none(type, external)
   private
   
   public :: ConservationMetadata
   public :: make_ConservationMetadata
   public :: operator(==)
   public :: operator(/=)
   
   type :: ConservationMetadata
      private
      logical :: is_mirror_ = .true.
      type(ConservationType) :: conservation_type = CONSERVE_NONE
      logical :: is_conservable = .false.
   contains
      procedure :: make_info
      procedure :: is_mirror
      procedure :: get_conservation_type
      procedure :: get_is_conservable
   end type ConservationMetadata
   
   interface ConservationMetadata
      module procedure new_ConservationMetadata
      module procedure new_ConservationMetadata_mirror
   end interface ConservationMetadata
   
   interface operator(==)
      module procedure equal_to
   end interface operator(==)
   
   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)
   
   ! Info keys
   character(*), parameter :: KEY_IS_MIRROR = "/is_mirror"
   character(*), parameter :: KEY_CONSERVATION_TYPE = "/conservation_type"
   character(*), parameter :: KEY_IS_CONSERVABLE = "/is_conservable"
   
contains

   function new_ConservationMetadata(conservation_type, is_conservable, unusable) result(metadata)
      type(ConservationMetadata) :: metadata
      type(ConservationType), intent(in) :: conservation_type
      logical, optional, intent(in) :: is_conservable
      class(*), optional, intent(in) :: unusable
      
      metadata%is_mirror_ = .false.
      metadata%conservation_type = conservation_type
      if (present(is_conservable)) metadata%is_conservable = is_conservable
      
      _UNUSED_DUMMY(unusable)
   end function new_ConservationMetadata
   
   function new_ConservationMetadata_mirror() result(metadata)
      type(ConservationMetadata) :: metadata
      metadata%is_mirror_ = .true.
   end function new_ConservationMetadata_mirror
   
   logical function is_mirror(this)
      class(ConservationMetadata), intent(in) :: this
      is_mirror = this%is_mirror_
   end function is_mirror
   
   function get_conservation_type(this) result(ctype)
      type(ConservationType) :: ctype
      class(ConservationMetadata), intent(in) :: this
      ctype = this%conservation_type
   end function get_conservation_type
   
   logical function get_is_conservable(this)
      class(ConservationMetadata), intent(in) :: this
      get_is_conservable = this%is_conservable
   end function get_is_conservable
   
   function make_info(this, rc) result(info)
      type(ESMF_Info) :: info
      class(ConservationMetadata), intent(in) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      
      info = ESMF_InfoCreate(_RC)
      
      call MAPL_InfoSet(info, key=KEY_IS_MIRROR, value=this%is_mirror(), _RC)
      _RETURN_IF(this%is_mirror())
      
      ! Write conservation properties
      call MAPL_InfoSet(info, KEY_CONSERVATION_TYPE, this%conservation_type%to_string(), _RC)
      call MAPL_InfoSet(info, KEY_IS_CONSERVABLE, this%is_conservable, _RC)
      
      _RETURN(_SUCCESS)
   end function make_info
   
   function make_ConservationMetadata(info, key, rc) result(metadata)
      type(ConservationMetadata) :: metadata
      type(ESMF_Info), intent(in) :: info
      character(*), optional, intent(in) :: key
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: full_key, str_value
      logical :: is_mirror, is_present
      logical :: is_conservable_buffer
      
      ! Check for mirror
      is_mirror = .FALSE.
      full_key = KEY_IS_MIRROR
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, key=full_key, value=is_mirror, _RC)
      end if
      
      if (is_mirror) then
         metadata = ConservationMetadata() ! mirror constructor
         _RETURN(_SUCCESS)
      end if
      
      ! Not a mirror - set is_mirror_ to false
      metadata%is_mirror_ = .false.
      
      ! Read conservation_type
      full_key = KEY_CONSERVATION_TYPE
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, full_key, str_value, _RC)
         metadata%conservation_type = ConservationType(str_value)
      end if
      
      ! Read is_conservable
      full_key = KEY_IS_CONSERVABLE
      if (present(key)) full_key = key // full_key
      is_present = ESMF_InfoIsPresent(info, key=full_key, _RC)
      if (is_present) then
         call MAPL_InfoGet(info, full_key, is_conservable_buffer, _RC)
         metadata%is_conservable = is_conservable_buffer
      end if
      
      _RETURN(_SUCCESS)
   end function make_ConservationMetadata
   
   logical function equal_to(lhs, rhs) result(equals)
      type(ConservationMetadata), intent(in) :: lhs
      type(ConservationMetadata), intent(in) :: rhs
      
      equals = .false.
      
      if (lhs%is_mirror() .neqv. rhs%is_mirror()) return
      if (lhs%is_mirror()) then
         equals = .true.
         return
      end if
      
      if (lhs%conservation_type /= rhs%conservation_type) return
      if (lhs%is_conservable .neqv. rhs%is_conservable) return
      
      equals = .true.
   end function equal_to
   
   logical function not_equal_to(lhs, rhs) result(not_equals)
      type(ConservationMetadata), intent(in) :: lhs
      type(ConservationMetadata), intent(in) :: rhs
      not_equals = .not. (lhs == rhs)
   end function not_equal_to

end module mapl3g_ConservationMetadata

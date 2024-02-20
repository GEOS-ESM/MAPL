module hconfig_utils

!_   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   use hconfig_value_base
   use hconfig_value_i4
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none
   private
!_   public ::
!_ INTERFACES
   
   interface HConfigValue
      module procedure :: construct_hconfig_value
   end interface HConfigValue

   interface get_value
   end interface get_value
!_ TYPES
!_ VARIABLES
contains

   function construct_hconfig_value(hconfig, keystring, value, default) result(hv)
      class(HConfigValue) :: hv
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      class(*), intent(in) :: value
      class(*), optional :: default
      class(HConfigValue) :: hv

      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         hv = HConfigValueI4(default)
         hv%typestring_ = 'I4'
      type is (integer(kind=ESMF_KIND_I8))
         hv = HConfigValueI8(default)
         hv%typestring_ = 'I8'
      type is (real(kind=ESMF_KIND_R4))
         hv = HConfigValueR4(default)
         hv%typestring_ = 'R4'
      type is (real(kind=ESMF_KIND_R8))
         hv = HConfigValueR8(default)
         hv%typestring_ = 'R8'
      type is (logical)
         hv = HConfigValueLogical(default)
         hv%typestring_ = 'L'
      type is (character(len=*))
         hv = HConfigValueString(default)
         hv%typestring_ = 'CH'
      class default
         _FAIL('Unsupported type for conversion')
      end select

      hv%hconfig_ = hconfig
      hv%keystring_ = keystring
      hv%keystring_found = ESMF_HConfigIsDefined(this%hconfig_, keyString=keystring, rc=status)
      hv%last_status_ = status

   end construct_hconfig_value

   subroutine get_value_common(hv, value, rc)
      class(HConfigValue), intent(in) :: hv
      class(*), intent(out) :: value
      integer, optional, intent(out) :: rc
      integer :: status

      if(.not. hv%value_is_set()) then
         call hv%set_value(_RC)
      end if

      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         hv = HConfigValueI4(default)
      type is (integer(kind=ESMF_KIND_I8))
         hv = HConfigValueI8(default)
      type is (real(kind=ESMF_KIND_R4))
         hv = HConfigValueR4(default)
      type is (real(kind=ESMF_KIND_R8))
         hv = HConfigValueR8(default)
      type is (logical)
         hv = HConfigValueLogical(default)
      type is (character(len=*))
         hv = HConfigValueString(default)
      class default
         _FAIL('Unsupported type for conversion')
      end select
      

   subroutine get_value_i4(hv, value, rc)
      class(HConfigValueI4), intent(in) :: hv
      integer(kind=int32), intent(out) :: value
      integer, optional, intent(out) :: rc
      integer :: status

      if(.not. hv%value_is_set()) then
         call hv%set_value(rc)
   
!   subroutine get_hconfig_value(hconfig, keystring, value, value, unusable, default, rc)
!      type(ESMF_HConfig), intent(inout) :: hconfig
!      character(len=*), intent(in) :: keystring
!      class(*), intent(inout) :: value
!      class(KeywordEnforcer), optional, intent(in) :: unusable
!      class(*), optional :: default
!      integer, optional, intent(out) :: rc
!      class(HConfigValue) :: value
end module hconfig_utils

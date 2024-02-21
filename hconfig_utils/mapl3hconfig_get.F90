!#include "MAPL_ErrLog.h"
module mapl3hconfig_get

   use mapl3hconfig_get_private, only: MAPL_HConfigGet => get_value
!   use mapl_ErrorHandling
!   use mapl_KeywordEnforcer
!   use :: esmf, only: ESMF_HConfig

   implicit none

   private

   public :: MAPL_HConfigGet

!   interface MAPL_HConfigGet
!      module procedure :: hconfig_get_scalar
!   end interface MAPL_HConfigGet

!contains

!   subroutine hconfig_get_scalar(hconfig, keystring, value, unusable, found, default, equals_default, typestring, valuestring, rc)
!      type(ESMF_HConfig), intent(inout) :: hconfig
!      character(len=*), intent(in) :: keystring
!      class(*), intent(inout) :: value
!      class(KeywordEnforcer), optional, intent(in) :: unusable
!      logical, optional, intent(out) :: found
!      class(*), optional, intent(inout) :: default
!      logical, optional, intent(out) :: equals_default
!      character(len=:), optional, allocatable, intent(inout) :: typestring
!      character(len=:), optional, allocatable, intent(inout) :: valuestring
!      integer, optional, intent(out) :: rc
!      integer :: status
!      logical :: found_
!
!      call get_value(hconfig, value, keystring, found=found_, &
!         default=default, equals_default=equals_default, &
!         typestring=typestring, valuestring=valuestring, _RC)
!      _ASSERT(found_ .or. present(found), 'Keystring "' // trim(keystring) // '" not found')
!
!      _RETURN(_SUCCESS)
!      _UNUSED_DUMMY(unusable)
!
!   end subroutine hconfig_get_scalar

end module mapl3hconfig_get

#include "MAPL_ErrLog.h"
module mapl3hconfig_get

   use mapl3hconfig_get_private, only: get_value
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use :: esmf, only: ESMF_HConfig

   implicit none

   private

   public :: MAPL_HConfigGet

   interface MAPL_HConfigGet
      module procedure :: hconfig_get_scalar
   end interface MAPL_HConfigGet

contains

   subroutine hconfig_get_scalar(hconfig, keystring, value, unusable, found, typestring, valuestring, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      class(*), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      character(len=:), optional, allocatable, intent(inout) :: typestring
      character(len=:), optional, allocatable, intent(inout) :: valuestring
      integer, optional, intent(out) :: rc
      integer :: status

      call get_value(hconfig, value, keystring, found=found, &
         typestring=typestring, valuestring=valuestring, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine hconfig_get_scalar

end module mapl3hconfig_get

#include "MAPL_ErrLog.h"
module mapl3hconfig_get

   use mapl3hconfig_get_private
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none

   private

   public :: MAPL_HConfigGet

   interface MAPL_HConfigGet
      module procedure :: hconfig_get_scalar
   end interface MAPL_HConfigGet

contains

   subroutine hconfig_get_scalar(hconfig, keystring, value, message, unusable, value_is_set, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      class(*), intent(inout) :: value
      character(len=:), allocatable, intent(inout) :: message
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_is_set
      integer, optional, intent(out) :: rc
      logical :: found

      _UNUSED_DUMMY(unusable)

      call get_value(hconfig, value, found, message, keystring, _RC)
      if(present(value_is_set)) value_is_set = found

      _RETURN(_SUCCESS)

   end subroutine hconfig_get_scalar

end module mapl3hconfig_get

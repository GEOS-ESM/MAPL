#include "MAPL_ErrLog.h"

module mapl3g_HConfigUtils
   use esmf
   use mapl_ErrorHandlingMod
   implicit none

   public :: MAPL_GetResource

   interface MAPL_GetResource
      procedure get_string
      procedure get_i4
      procedure get_i4seq
   end interface MAPL_GetResource

contains
   
   subroutine get_string(s, hconfig, key, default, rc)
      character(:), allocatable, intent(out) :: s
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      character(*), intent(in) :: default
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      s = default

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _RETURN_UNLESS(found)

      s = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_string


   subroutine get_i4(i, hconfig, key, default, rc)
      integer(kind=ESMF_KIND_I4), intent(out) :: i
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: default
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      i = default

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _RETURN_UNLESS(found)

      i = ESMF_HConfigAsI4(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_i4


   subroutine get_i4seq(i4seq, hconfig, key, rc)
      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: i4seq(:)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      allocate(i4seq(0), _STAT)

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _RETURN_UNLESS(found)

      i4seq = ESMF_HConfigAsI4Seq(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_i4seq


end module mapl3g_HConfigUtils

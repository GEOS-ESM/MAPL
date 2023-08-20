#include "MAPL_ErrLog.h"

module mapl3g_HConfigUtils
   use mapl_ErrorHandlingMod
   use esmf
   implicit none

   public :: MAPL_GetResource

   interface MAPL_GetResource
      procedure get_string
      procedure get_i4
      procedure get_logical
      procedure get_i4seq
      procedure get_r4seq
   end interface MAPL_GetResource

contains
   
   subroutine get_string(value, hconfig, key, default, rc)
      character(:), allocatable, intent(out) :: value
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      if (present(default)) value = default

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _ASSERT(found .or. present(default), 'Key "'//trim(key)//'" not found in config file')
      _RETURN_UNLESS(found)
      
      value = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_string


   subroutine get_i4(value, hconfig, key, default, rc)
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      if (present(default)) value = default

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _ASSERT(found .or. present(default), 'Key "'//trim(key)//'" not found in config file')
      _RETURN_UNLESS(found)

      value = ESMF_HConfigAsI4(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_i4

   subroutine get_logical(value, hconfig, key, default, rc)
      logical, intent(out) :: value
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      logical, optional, intent(in) :: default
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      if (present(default)) value = default

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _ASSERT(found .or. present(default), 'Key "'//trim(key)//'" not found in config file')
      _RETURN_UNLESS(found)

      value = ESMF_HConfigAsLogical(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_logical


   subroutine get_i4seq(values, hconfig, key, default, rc)
      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: values(:)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default(:)

      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      if (present(default)) values = default

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _ASSERT(found .or. present(default), 'Key "'//trim(key)//'" not found in config file')
      _RETURN_UNLESS(found)

      values = ESMF_HConfigAsI4Seq(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_i4seq

   subroutine get_r4seq(values, hconfig, key, default, rc)
      real(kind=ESMF_KIND_R4), allocatable, intent(out) :: values(:)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), optional, intent(in) :: default(:)

      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      if (present(default)) values = default

      found = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
      _ASSERT(found .or. present(default), 'Key "'//trim(key)//'" not found in config file')
      _RETURN_UNLESS(found)

      values = ESMF_HConfigAsR4Seq(hconfig, keystring=key, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_r4seq


end module mapl3g_HConfigUtils

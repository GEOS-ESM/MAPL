#include "MAPL_ErrLog.h"
module mapl3hconfig_get_private
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined, MAXSTRLEN => ESMF_MAXSTR
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_KIND_I4, ESMF_HConfigAsI8, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_KIND_R4, ESMF_HConfigAsR8, ESMF_KIND_R8
   use :: esmf, only: ESMF_HConfigAsLogical, ESMF_HConfigAsString
   use mapl_ErrorHandling

   implicit none

   public :: MAXSTRLEN
   public :: get_value

   interface get_value
      module procedure :: get_value_scalar
   end interface get_value

   character(len=*), parameter :: TYPESTRING_I4 = 'I4'
   character(len=*), parameter :: TYPESTRING_I8 = 'I8'
   character(len=*), parameter :: TYPESTRING_R4 = 'R4'
   character(len=*), parameter :: TYPESTRING_R8 = 'R8'
   character(len=*), parameter :: TYPESTRING_L = 'L'
   character(len=*), parameter :: TYPESTRING_CH = 'CH'

contains
      
   subroutine get_value_scalar(hconfig, value, keystring, unusable, found, typestring, valuestring, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      class(*), intent(inout) :: value
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      character(len=:), allocatable, optional, intent(inout) :: typestring
      character(len=:), allocatable, optional, intent(inout) :: valuestring
      integer, intent(out) :: rc

      integer :: status
      integer :: ios
      character(len=MAXSTRLEN) :: rawstring
      character(len=:), allocatable :: typestring_
      character(len=:), allocatable :: valuestring_
      logical :: is_found

      is_found = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
      if(.not. is_found) then
         _ASSERT(present(found), 'Key "' // trim(keystring) '" was not found.')
         _RETURN(_SUCCESS)
      end if

      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         value = ESMF_HConfigAsI4(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(I12)', iostat=ios) value
         typestring_ = TYPESTRING_I4
      type is (integer(kind=ESMF_KIND_I8))
         value = ESMF_HConfigAsI8(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(I22)', iostat=ios) value
         typestring_ = TYPESTRING_I8
      type is (real(kind=ESMF_KIND_R4))
         value = ESMF_HConfigAsR4(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(G17.8)', iostat=ios) value
         typestring_ = TYPESTRING_R4
      type is (real(kind=ESMF_KIND_R8))
         value = ESMF_HConfigAsR8(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(G24.16)', iostat=ios) value
         typestring_ = TYPESTRING_R8
      type is (logical)
         value = ESMF_HConfigAsLogical(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(L1)', iostat=ios) value
         typestring_ = TYPESTRING_L
      type is (character(len=*))
         value = ESMF_HConfigAsString(hconfig, keyString=keystring, _RC)
         rawstring = value
         typestring_ = TYPESTRING_CH
      class default
         _FAIL('Unsupported type for conversion')
      end select
      _ASSERT(ios == 0, 'Failed to write value to rawstring')
      valuestring_ = trim(adjustl(rawstring))
      _ASSERT(len(valuestring) > 0, 'valuestring is empty.')
      if(present(valuestring)) valuestring = valuestring_
      if(present(typestring)) typestring = typestring_
      if(present(found)) found = is_found
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_value_scalar

end module mapl3hconfig_get_private

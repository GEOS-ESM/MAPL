#include "MAPL_ErrLog.h"
module mapl3hconfig_get_private
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined, MAXSTRLEN => ESMF_MAXSTR
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_KIND_I4, ESMF_HConfigAsI8, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_KIND_R4, ESMF_HConfigAsR8, ESMF_KIND_R8
   use :: esmf, only: ESMF_HConfigAsLogical, ESMF_HConfigAsString
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none

   public :: MAXSTRLEN
   public :: get_value

contains

   subroutine get_value(hconfig, value, found, message, keystring, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      class(*), intent(inout) :: value
      logical, intent(out) :: found
      character(len=:), allocatable, intent(inout) :: message
      character(len=*), intent(in) :: keystring
      integer, intent(out) :: rc

      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring

      integer :: status
      integer :: ios
      character(len=MAXSTRLEN) :: rawstring

      found = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
      if(.not. found) then
         _RETURN(_SUCCESS)
      end if

      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         typestring = 'I4'
         value = ESMF_HConfigAsI4(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(I12)', iostat=ios) value
      type is (integer(kind=ESMF_KIND_I8))
         typestring = 'I8'
         value = ESMF_HConfigAsI8(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(I22)', iostat=ios) value
      type is (real(kind=ESMF_KIND_R4))
         typestring = 'R4'
         value = ESMF_HConfigAsR4(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(G17.8)', iostat=ios) value
      type is (real(kind=ESMF_KIND_R8))
         typestring = 'R8'
         value = ESMF_HConfigAsR8(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(G24.16)', iostat=ios) value
      type is (logical)
         typestring = 'L'
         value = ESMF_HConfigAsLogical(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt='(L1)', iostat=ios) value
      type is (character(len=*))
         typestring = 'CH'
         value = ESMF_HConfigAsString(hconfig, keyString=keystring, _RC)
         rawstring = value
      class default
         _FAIL('Unsupported type for conversion')
      end select
      _ASSERT(ios == 0, 'Failed to write value to rawstring')
      valuestring = trim(adjustl(rawstring))
      _ASSERT(len(valuestring) > 0, 'valuestring is empty.')
      message = form_message(typestring, keystring, valuestring, valuerank=0)
      _ASSERT(len(message) > 0, 'message is empty.')
      
      _RETURN(_SUCCESS)

   end subroutine get_value

   function form_message(typestring, keystring, valuestring, valuerank) result(message)
      character(len=:), allocatable :: message
      character(len=*), intent(in) :: typestring
      character(len=*), intent(in) :: keystring
      character(len=*), intent(in) :: valuestring
      integer, intent(in) :: valuerank
      character(len=*), parameter :: J_ = ', '

      if(valuerank > 0) then
         message = typestring //J_// keystring //J_// valuestring //J_// rankstring(valuerank)
      else
         message = typestring //J_// keystring //J_// valuestring
      end if

   end function form_message
      
   function rankstring(valuerank) result(string)
      character(len=:), allocatable :: string
      integer, intent(in) :: valuerank

      if(valuerank > 0) then
         string = '(:' // repeat(',:', valuerank-1) // ')'
      else
         string = ''
      end if

   end function rankstring

end module mapl3hconfig_get_private

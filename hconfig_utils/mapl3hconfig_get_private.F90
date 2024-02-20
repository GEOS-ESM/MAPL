#include "MAPL_ErrLog.h"
module mapl3hconfig_get_private
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined, MAXSTRLEN => ESMF_MAXSTR
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_KIND_I4, ESMF_HConfigAsI8, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_KIND_R4, ESMF_HConfigAsR8, ESMF_KIND_R8
   use :: esmf, only: ESMF_HConfigAsLogical, ESMF_HConfigAsString
   use mapl_KeywordEnforcer
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
      
   subroutine get_value_scalar(hconfig, keystring, value, found, unusable, default, equals_default, typestring, valuestring, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      class(*), intent(inout) :: value
      logical, intent(out) :: found
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(inout) :: default
      logical, optional, intent(out) :: equals_default
      character(len=:), allocatable, optional, intent(inout) :: typestring
      character(len=:), allocatable, optional, intent(inout) :: valuestring
      integer, intent(out) :: rc

      integer :: status
      integer :: ios
      character(len=MAXSTRLEN) :: rawstring
      character(len=:), allocatable :: typestring_
      character(len=:), allocatable :: valuestring_

      _ASSERT(.not. (present(equals_default) .and. .not. present(default)), 'equals_default requires default')
      found = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
      _RETURN_UNLESS(found .or. present(default))

      ! fct(hconfig, keystring, value, found, typestring, valuestring, default, rc)
!         type(ESMF_HConfig), intent(inout) :: hconfig
!         character(len=*), intent(in) :: keystring
!         integer(kind=ESMF_KIND_I4), intent(out) :: value
!         logical, intent(inout) :: found
!         character(len=:), allocatable, intent(out) :: typestring
!         character(len=:), allocatable, intent(out) :: valuestring
!         class(*), optional, intent(in) :: default
!         integer, optional, intent(out) :: rc
!         integer :: status

         ! found and present(default): get hconfig & compare
         ! not found and present(default): value = default & compare true
         ! found and not(present(default)): get hconfig & compare false
         ! not found and not(present(default)): error
         if(found) then
            value = ESMF_HConfigAsI4(hconfig, keystring=keystring, _RC)
         end if
         if(present(default)) then
            select type(default)
            type is (integer(kind=ESMF_KIND_I4))


      if(present(default)) then
         select type(value)
         type is (integer(kind=ESMF_KIND_I4))
            select type(default)
            type is (integer(kind=ESMF_KIND_I4))
               value = default
            end select
         type is (integer(kind=ESMF_KIND_I8))
            select type(default)
            type is (integer(kind=ESMF_KIND_I8))
               value = default
            end select
         type is (real(kind=ESMF_KIND_R4))
            select type(default)
            type is (integer(kind=ESMF_KIND_R4))
               value = default
            end select
         type is (real(kind=ESMF_KIND_R8))
            select type(default)
            type is (integer(kind=ESMF_KIND_R8))
               value = default
            end select
         type is (logical)
            select type(default)
            type is (logical)
               value = default
            end select
         type is (character(len=*))
            select type(default)
            type is (character(len=*))
               value = default
            end select
         class default
            _FAIL('Unsupported type for conversion')
         end select
      else
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
      end if

      _ASSERT(ios == 0, 'Failed to write value to rawstring')
      valuestring_ = trim(adjustl(rawstring))
      _ASSERT(len(valuestring) > 0, 'valuestring is empty.')
      if(present(valuestring)) valuestring = valuestring_
      if(present(typestring)) typestring = typestring_
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_value_scalar

end module mapl3hconfig_get_private

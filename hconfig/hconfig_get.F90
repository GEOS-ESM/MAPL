#if defined TYPE_I4
#undef TYPE_I4
#endif

#if defined TYPE_I8
#undef TYPE_I8
#endif

#if defined TYPE_R4
#undef TYPE_R4
#endif

#if defined TYPE_R8
#undef TYPE_R8
#endif

#if defined TYPE_LOGICAL
#undef TYPE_LOGICAL
#endif

#if defined TYPE_CHARACTER
#undef TYPE_CHARACTER
#endif

#define TYPE_I4 integer(kind=ESMF_KIND_I4)
#define TYPE_I8 integer(kind=ESMF_KIND_I8)
#define TYPE_R4 real(kind=ESMF_KIND_R4)
#define TYPE_R8 real(kind=ESMF_KIND_R8)
#define TYPE_LOGICAL logical
#define TYPE_CHARACTER character(len=*)

#include "MAPL_ErrLog.h"
! This module uses macros to represent data types that are used frequently.
! These macros are used below for type of values
module hconfig_get
   use :: esmf, only: ESMF_HConfig
   use :: esmf, only: ESMF_HConfigIsDefined
   use :: esmf, only: ESMF_HConfigAsString
   use :: esmf, only: ESMF_HConfigAsLogical
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_KIND_I4
   use :: esmf, only: ESMF_HConfigAsI8, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_KIND_R4
   use :: esmf, only: ESMF_HConfigAsR8, ESMF_KIND_R8
   use :: esmf, only: MAXSTRLEN => ESMF_MAXSTR
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none

   public :: MAXSTRLEN
   public :: get_value

   character(len=*), parameter :: FMTI4 = '(I12)'
   character(len=*), parameter :: FMTI8 = '(I22)'
   character(len=*), parameter :: FMTR4 = '(G17.8)'
   character(len=*), parameter :: FMTR8 = '(G24.16)'
   character(len=*), parameter :: FMTL = '(L1)'

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
      logical :: hconfig_is_not_defined
      integer :: ios
      character(len=MAXSTRLEN) :: rawstring

      found = .FALSE.

      hconfig_is_not_defined = .not. ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)

      if(hconfig_is_not_defined) then
         _RETURN(_SUCCESS)
      end if

      select type(value)
      type is (TYPE_I4)
         typestring = 'I4'
         value = ESMF_HConfigAsI4(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt=FMTI4, iostat=ios) value
      type is (TYPE_I8)
         typestring = 'I8'
         value = ESMF_HConfigAsI8(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt=FMTI8, iostat=ios) value
      type is (TYPE_R4)
         typestring = 'R4'
         value = ESMF_HConfigAsR4(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt=FMTR4, iostat=ios) value
      type is (TYPE_R8)
         typestring = 'R8'
         value = ESMF_HConfigAsR8(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt=FMTR8, iostat=ios) value
      type is (TYPE_LOGICAL)
         typestring = 'L'
         value = ESMF_HConfigAsLogical(hconfig, keyString=keystring, _RC)
         write(rawstring, fmt=FMTL, iostat=ios) value
      type is (TYPE_CHARACTER)
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
      found = .TRUE.
      
      _RETURN(_SUCCESS)

   end subroutine get_value

   function form_message(typestring, keystring, valuestring, valuerank) result(message)
      character(len=:), allocatable :: message
      character(len=*), intent(in) :: typestring
      character(len=*), intent(in) :: keystring
      character(len=*), intent(in) :: valuestring
      integer, intent(in) :: valuerank
      character(len=:), allocatable :: rank_string
      character(len=MAXSTRLEN) :: rawstring
      character(len=*), parameter :: FMT3 = '(A,", ", A, ", ", A)'
      character(len=*), parameter :: FMT4 = '(A,", ", A, ", ", A, ", ", A)'
      integer :: ios

      if(valuerank > 0) then
         write(rawstring, fmt=FMT4, iostat=ios) typestring, keystring, valuestring, rankstring(valuerank)
      else
         write(rawstring, fmt=FMT3, iostat=ios) typestring, keystring, valuestring
      end if

      if(ios == 0) then
         message = trim(rawstring)
      else
         message = ''
      end if

   end function form_message
      
   function rankstring(valuerank) result(string)
      character(len=:), allocatable :: string
      integer, intent(in) :: valuerank
      character(len=*), parameter :: OPEN_STRING = '(:'
      character(len=*), parameter :: CLOSE_STRING = ')'
      character(len=*), parameter :: ADDITIONAL_RANK = ',:'
      character(len=MAXSTRLEN) :: raw = ''

      if(valuerank > 0) raw = OPEN_STRING // repeat(ADDITIONAL_RANK, valuerank-1) // CLOSE_STRING
      string = trim(raw)

   end function rankstring

end module hconfig_get
!   subroutine get_i4(hconfig, value, found, message, keystring, rc)
!      ! Dummy argument names are boilerplate.
!      type(ESMF_HConfig), intent(inout) :: hconfig
!      TYPE_I4, intent(inout) :: value ! wdb TYPE SPECIFIC
!      logical, intent(out) :: found
!      character(len=:), allocatable, intent(inout) :: message
!      character(len=*), intent(in) :: keystring
!      integer, intent(out) :: rc
!
!      character(len=:), allocatable :: typestring
!      character(len=:), allocatable :: valuestring
!
!      integer :: status
!      logical :: is_defined
!
!      found = .FALSE.
!      is_defined = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
!      if (is_defined) then
!         value = ESMF_HConfigAsI4(hconfig, keyString=keystring, _RC) !wdb TYPE SPECIFIC
!         valuestring = make_valuestring(value)
!         _ASSERT(len(valuestring) > 0, 'valuestring is empty.')
!         typestring = get_typestring(value)
!         _ASSERT(len(typestring) > 0, 'typestring is empty.')
!         message = form_message(typestring, keystring, valuestring, valuerank=0)
!         _ASSERT(len(message) > 0, 'message is empty.')
!         found = .TRUE.
!      else
!         message = ''
!      end if
!      
!      _RETURN(_SUCCESS)
!
!   end subroutine get_i4
!
!   subroutine get_r4(hconfig, value, found, message, keystring, rc)
!      ! Dummy argument names are boilerplate.
!      type(ESMF_HConfig), intent(inout) :: hconfig
!      TYPE_R4, intent(inout) :: value ! wdb TYPE SPECIFIC
!      logical, intent(out) :: found
!      character(len=:), allocatable, intent(inout) :: message
!      character(len=*), intent(in) :: keystring
!      integer, intent(out) :: rc
!
!      character(len=:), allocatable :: typestring
!      character(len=:), allocatable :: valuestring
!
!      integer :: status
!      logical :: is_defined
!
!      found = .FALSE.
!      is_defined = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
!      if (is_defined) then
!         value = ESMF_HConfigAsR4(hconfig, keyString=keystring, _RC) !wdb TYPE SPECIFIC
!         valuestring = make_valuestring(value)
!         _ASSERT(len(valuestring) > 0, 'valuestring is empty.')
!         typestring = get_typestring(value)
!         _ASSERT(len(typestring) > 0, 'typestring is empty.')
!         message = form_message(typestring, keystring, valuestring, valuerank=0)
!         _ASSERT(len(message) > 0, 'message is empty.')
!         found = .TRUE.
!      else
!         message = ''
!      end if
!
!      _RETURN(_SUCCESS)
!
!   end subroutine get_r4

!   subroutine get_string(hconfig, value, found, message, keystring, rc)
!      ! Dummy argument names are boilerplate.
!      type(ESMF_HConfig), intent(inout) :: hconfig
!      TYPE_CHARACTER, intent(inout) :: value ! wdb TYPE SPECIFIC
!      logical, intent(out) :: found
!      character(len=:), allocatable, intent(inout) :: message
!      character(len=*), intent(in) :: keystring
!      integer, intent(out) :: rc
!
!      character(len=:), allocatable :: typestring
!      character(len=:), allocatable :: valuestring
!
!      integer :: status
!      logical :: is_defined
!
!      found = .FALSE.
!      is_defined = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
!      if (is_defined) then
!         value = ESMF_HConfigAsString(hconfig, keyString=keystring, _RC) !wdb TYPE SPECIFIC
!         valuestring = make_valuestring(value)
!         _ASSERT(len(valuestring) > 0, 'valuestring is empty.')
!         typestring = get_typestring(value)
!         _ASSERT(len(typestring) > 0, 'typestring is empty.')
!         message = form_message(typestring, keystring, valuestring, valuerank=0)
!         _ASSERT(len(message) > 0, 'message is empty.')
!         found = .TRUE.
!      else
!         message = ''
!      end if
!      
!      _RETURN(_SUCCESS)
!
!   end subroutine get_string

!   function make_valuestring(value) result(valuestring)
!      class(*), intent(in) :: value
!      character(len=:), allocatable :: valuestring
!      character(len=80) :: rawstring
!      integer :: ios
!
!      select type(value)
!      type is (TYPE_I4)
!         write(rawstring, fmt=FMTI4, iostat=ios) value
!      type is (TYPE_I8)
!         write(rawstring, fmt=FMTI8, iostat=ios) value
!      type is (TYPE_R4)
!         write(rawstring, fmt=FMTR4, iostat=ios) value
!      type is (TYPE_R8)
!         write(rawstring, fmt=FMTR8, iostat=ios) value
!      type is (TYPE_LOGICAL)
!         write(rawstring, fmt=FMTL, iostat=ios) value
!      type is (TYPE_CHARACTER)
!         rawstring = value
!      end select
!
!      if(ios == 0) then
!         valuestring = trim(adjustl(rawstring))
!      else
!         valuestring = ''
!      end if
!
!   end function make_valuestring

!   function get_typestring(value) result(typestring)
!      character(len=2) :: typestring
!      class(*), intent(in) :: value
!
!      typestring = ''
!      select type(value)
!      type is (TYPE_I4)
!         typestring = 'I4'
!      type is (TYPE_I8)
!         typestring = 'I8'
!      type is (TYPE_R4)
!         typestring = 'R4'
!      type is (TYPE_R8)
!         typestring = 'R8'
!      type is (TYPE_LOGICAL)
!         typestring = 'L'
!      type is (TYPE_CHARACTER)
!         typestring = 'CH'
!      end select
!
!   end function get_typestring

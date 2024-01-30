#include "MAPL_ErrLog.h"
#include "esmf_type_kind.h"
! This module uses macros to represent data types that are used frequently.
! These macros are used below for type of values
module hconfig_get_mod
   use :: esmf, only: ESMF_HConfig
   use :: esmf, only: ESMF_HConfigIsDefined
   use :: esmf, only: ESMF_HConfigAsString
   use :: esmf, only: ESMF_HConfigAsLogical
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_KIND_I4
   use :: esmf, only: ESMF_HConfigAsI8, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_KIND_R4
   use :: esmf, only: ESMF_HConfigAsR8, ESMF_KIND_R8
   use :: esmf, only: ESMF_TypeKind_Flag
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none

   interface GetHConfig
      module procedure :: get_i4
      module procedure :: get_i8
      module procedure :: get_r4
      module procedure :: get_r8
      module procedure :: get_logical
      module procedure :: get_string
   end interface GetHConfig

contains

   subroutine get_i4(hconfig, value, found, message, keystring, rc)
      ! Dummy argument names are boilerplate.
      type(ESMF_HConfig), intent(inout) :: hconfig
      TYPE_I4, intent(inout) :: value ! wdb TYPE SPECIFIC
      logical, intent(out) :: found
      character(len=:), allocatable, intent(inout) :: message
      character(len=*), intent(in) :: keystring
      integer, intent(out) :: rc

      logical, parameter :: IS_ARRAY = .FALSE.
      type(ESMF_TypeKind_Flag) :: typekind
      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring

      integer :: status
      logical :: is_defined

      found = .FALSE.
      is_defined = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
      if (is_defined) then
         value = ESMF_HConfigAsI4(hconfig, keyString=keystring, rc=status) !wdb TYPE SPECIFIC
         valuestring = ESMF_HConfigAsString(hconfig, keyString=keystring, _RC)
         found = .TRUE.
      end if

      typekind = get_esmf_typekind_flag(value, _RC)
      typestring = get_typestring(typekind, _RC
      message = form_message(typestring, keystring, valuestring, IS_ARRAY)

      _RETURN(_SUCCESS)

   end subroutine get_i4

   function form_message(typestring, keystring, valuestring, is_array) result(message)
      character(len=:), allocatable :: message
      character(len=*), intent(in) :: typestring
      character(len=*), intent(in) :: keystring
      character(len=*), intent(in) :: valuestring
      logical, optional, intent(in) :: is_array

      character(len=*), parameter :: JOIN = ', '

      character(len=*), parameter :: RANK1 = '(:)'
      character(len=*), parameter :: HIGHEST_RANK
      integer, parameter :: LEN_RANKSTRING = len(HIGHEST_RANK)
      character(len=LEN_RANKSTRING) :: RANK0 = ''
      character(len=LEN_RANKSTRING) :: rankstring

      rankstring = merge(&
         merge(&
            RANK1,&
            RANK0,&
            is_array),&
         RANK0,&
         is_present(is_array)&
      )

      rankstring = trim(rankstring_)

      message = typestring // JOIN // trim(rankstring) // JOIN //&
         keystring // JOIN // valuestring  

   end function form_message
      
end module hconfig_get_mod

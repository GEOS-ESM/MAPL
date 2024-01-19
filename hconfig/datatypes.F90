!#include "MAPL_ErrLog.h"
#include "datatypes.h"
! This module offers procedures for processing types with kind constants
! defined in ESMF and ESMF_TypeKindFlag
module datatypes_mod

!   use mapl_ErrorHandling
!   use :: esmf, only: ESMF_TypeKind_Flag
!   use :: esmf, only: ESMF_TYPEKIND_I4, ESMF_TYPEKIND_I8
!   use :: esmf, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
!   use :: esmf, only: ESMF_KIND_I4, ESMF_KIND_I8
!   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
!   use :: esmf, only: ESMF_TYPEKIND_LOGICAL, ESMF_TYPEKIND_CHARACTER

   implicit none

!   interface get_typestring
!      module procedure :: get_typestring_array
!   end interface get_typestring

contains

!   integer function get_tk_int(etkf)
!      type(ESMF_TypeKind_Flag), intent(in) :: etkf
!      get_tk_int = etkf
!   end function get_tk_int

!   function get_esmf_typekind_flag(value, rc) result(flag)
!      type(ESMF_TypeKind_Flag) :: flag
!      class(*), intent(in) :: value
!      integer, optional, intent(out) :: rc
!
!      integer :: status
!
!      select type(value)
!      type is (TYPE_I4)
!         flag = ESMF_TYPEKIND_I4
!      type is (TYPE_I8)
!         flag = ESMF_TYPEKIND_I8
!      type is (TYPE_R4)
!         flag = ESMF_TYPEKIND_R4
!      type is (TYPE_R8)
!         flag = ESMF_TYPEKIND_R8
!      type is (TYPE_LOGICAL)
!         flag = ESMF_TYPEKIND_LOGICAL
!      type is (TYPE_CHARACTER)
!         flag = ESMF_TYPEKIND_CHARACTER
!      class default
!         _FAIL('Unsupported type')
!      end select
!
!      _RETURN(_SUCCESS)
!
!   end function get_esmf_typekind_flag
      
   function get_typestring(value) result(typestring)
      character(len=2) :: typestring = ''
      class(*), intent(in) :: value
      character(len=2), parameter :: TYPESTRINGS(size(FLAGS)) = &
         [ character(len=2) :: 'I4', 'I8', 'R4', 'R8', 'L', 'CH' ]
      integer :: i

      select type(value)
      type is (TYPE_I4)
         typestring = 'I4'
      type is (TYPE_I8)
         typestring = 'I8'
      type is (TYPE_R4)
         typestring = 'R4'
      type is (TYPE_R8)
         typestring = 'R8'
      type is (TYPE_LOGICAL)
         typestring = 'L'
      type is (TYPE_CHARACTER)
         typestring = 'CH'
      end select

   end function get_typestring
         
end module datatypes_mod
!   function get_typestring_extended(typekind) result(typestring)
!      character(len=:), allocatable :: typestring
!      type(ESMF_TypeKind_Flag), intent(in) :: typekind
!
!      if(typekind == ESMF_TYPEKIND_CHARACTER) then
!         typestring = 'CH'
!      else if(typekind == ESMF_TYPEKIND_LOGICAL) then
!         typestring = 'L' 
!      else if(typekind == ESMF_TYPEKIND_I4) then
!         typestring = 'I4'
!      else if(typekind == ESMF_TYPEKIND_I8) then
!         typestring = 'I8'
!      else if(typekind == ESMF_TYPEKIND_R4) then
!         typestring = 'R4'
!      else if(typekind == ESMF_TYPEKIND_R8) then
!         typestring = 'R8'
!      else 
!         typestring = 'UN'
!      end if
!
!   end function get_typestring_extended

!   function get_esmf_typekind_flag_string(typekind) result(string)
!      character(len=:), allocatable :: string
!      type(ESMF_TypeKind_Flag), intent(in) :: typekind
!
!      string = typekind
!
!   end function get_esmf_typekind_flag_string
!
!   function strip_tk(typekind_string)  result(tk)
!      character(len=:), allocatable :: tk
!      character(len=*), intent(in) :: typekind_string
!
!      tk = typekind_string((index(typekind_string, '_', .TRUE.) + 1):)
!
!   end function strip_tk
!
!   function get_typestring_simple(typekind) result(typestring)
!      character(len=:), allocatable :: typestring
!      type(ESMF_TypeKind_Flag), intent(in) :: typekind
!
!      typestring = strip_tk(get_esmf_typekind_flag_string(typekind))
!   
!   end function get_typestring_simple

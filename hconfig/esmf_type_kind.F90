#include "MAPL_ErrLog.h"
#include "esmf_type_kind.h"
! This module offers procedures for processing types with kind constants
! defined in ESMF and ESMF_TypeKindFlag
module esmf_type_kind_mod

   use mapl_ErrorHandling
   use :: esmf, only: ESMF_TypeKind_Flag
   use :: esmf, only: ESMF_TYPEKIND_I4, ESMF_TYPEKIND_I8
   use :: esmf, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use :: esmf, only: ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use :: esmf, only: ESMF_TYPEKIND_LOGICAL, ESMF_TYPEKIND_CHARACTER

   implicit none

contains

   function get_esmf_typekind_flag(value, rc) result(flag)
      type(ESMF_TypeKind_Flag) :: flag
      class(*), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status

      select type(value)
      type is (TYPE_I4)
         flag = ESMF_TYPEKIND_I4
      type is (TYPE_I8)
         flag = ESMF_TYPEKIND_I8
      type is (TYPE_R4)
         flag = ESMF_TYPEKIND_R4
      type is (TYPE_R8)
         flag = ESMF_TYPEKIND_R8
      type is (TYPE_LOGICAL)
         flag = ESMF_TYPEKIND_LOGICAL
      type is (TYPE_CHARACTER)
         flag = ESMF_TYPEKIND_CHARACTER
      class default
         _FAIL('Unsupported type')
      end select

      _RETURN(_SUCCESS)

   end function get_esmf_typekind_flag
      
   function get_typestring(typekind, rc) result(typestring)
      character(len=:), allocatable :: typestring
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      integer, optional, intent(out) :: rc

      integer :: status

      select case(typekind)
         case (ESMF_TYPEKIND_I4)
            typestring = 'I4'
         case (ESMF_TYPEKIND_I8)
            typestring = 'I8'
         case (ESMF_TYPEKIND_R4)
            typestring = 'R4'
         case (ESMF_TYPEKIND_R8)
            typestring = 'R8'
         case (ESMF_TYPEKIND_LOGICAL)
            typestring = 'L' 
         case (ESMF_TYPEKIND_CHARACTER)
            typestring = 'CH'
         case default
            _FAIL('Unsupported type')
      end select

      _RETURN(_SUCCESS)

   end function get_typestring

end module esmf_type_kind_mod

#include "MAPL_Generic.h"
module esmf_typekind_mod

   use mapl_ErrorHandling
   use esmf
   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

   implicit none

   private

   public :: get_esmf_typekind

   interface get_esmf_typekind
      module procedure :: get_esmf_typekind_scalar
   end interface get_esmf_typekind

contains

   function get_esmf_typekind_scalar(value, rc) result(tk)
      type(ESMF_TypeKind_Flag) :: esmftk
      class(*), intent(in) :: value
      integer, optional, intent(out) :: rc
      integer :: status

      select type(value)
      type is (character(len=*))
         esmftk = ESMF_TYPEKIND_CHARACTER
      type is (logical)
         esmftk = ESMF_TYPEKIND_LOGICAL
      type is (integer(kind=int32)
         esmftk = ESMF_TYPEKIND_I4
      type is (integer(kind=int64)
         esmftk = ESMF_TYPEKIND_I8
      type is (real(kind=real32)
         esmftk = ESMF_TYPEKIND_I4
      type is (real(kind=real64)
         esmftk = ESMF_TYPEKIND_I8
      case default
         _FAIL('Unknown ESMF_TypeKindFlag')
      end select
      _RETURN(_SUCCESS)

   end function get_esmf_typekind_scalar

end module esmf_typekind_mod



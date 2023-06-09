#include "MAPL_Generic.h"

module MAPL_FieldBinaryOperations
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public fieldAdd
   public fieldSubtract
   public fieldDivide
   public fieldMultiply

   contains

#define _OP +
#define _FUNC Add
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

#define _OP -
#define _FUNC Subtract
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

#define _OP *
#define _FUNC Multiply
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

#define _OP /
#define _FUNC Divide
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

end module MAPL_FieldBinaryOperations

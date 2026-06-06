#include "MAPL.h"

module mapl_FieldBinaryOperations_mod
   use ESMF
   use mapl_ErrorHandling_mod
   use mapl_FieldPointerUtilities_mod
   implicit none
   private

   public fieldAdd
   public fieldSubtract
   public fieldDivide
   public fieldMultiply
   public fieldPower

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

#define _OP **
#define _FUNC Power
#include "FieldBinaryOperatorTemplate.H"
#undef _OP
#undef _FUNC

end module mapl_FieldBinaryOperations_mod

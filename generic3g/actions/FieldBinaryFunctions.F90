#include "MAPL_Generic.h"

module MAPL_FieldBinaryFunctions
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use mapl3g_AccumulatorConstants

   implicit none
   private

   public :: fieldMin
   public :: fieldMax
   public :: fieldAdd
   public :: fieldDivide

   interface fieldAdd
      module procedure :: add_r4
      module procedure :: add_r8
   end interface fieldAdd

   interface fieldDivide
      module procedure :: divide_r4
      module procedure :: divide_r8
   end interface fieldDivide

contains

#define _FUNC add
#define _OP +
#include "OperatorTemplate.H"
#undef _OP
#undef _FUNC
#define _FUNC divide
#define _OP /
#define CHECK_ZEROES
#include "OperatorTemplate.H"
#undef CHECK_ZEROES
#undef _OP
#undef _FUNC

#define _KEEP_LEFT

#define _FUNC Min
#include "FieldBinaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Min
#define _KEEP_LEFT
#include "FieldBinaryFunctionTemplate.H"
#undef _FUNC

#undef _KEEP_LEFT

#define _FUNC add
#include "FieldBinaryFunctionTemplate.H"
#undef _FUNC

end module MAPL_FieldBinaryFunctions

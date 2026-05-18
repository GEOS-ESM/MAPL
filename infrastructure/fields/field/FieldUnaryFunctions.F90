#include "MAPL.h"

module MAPL_FieldUnaryFunctions
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   implicit none
   private

   public :: fieldAbs
   public :: fieldExp
   public :: fieldLog10
   public :: fieldLog
   public :: fieldSqrt
   public :: fieldSinh
   public :: fieldCosh
   public :: fieldTanh
   public :: fieldSin
   public :: fieldCos
   public :: fieldTan
   public :: fieldAsin
   public :: fieldAcos
   public :: fieldAtan

contains

#define _FUNC Abs
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Exp
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Log10
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Log
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Sqrt
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Sinh
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Cosh
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Tanh
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Sin
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Cos
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Tan
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Asin
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Acos
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Atan
#include "FieldUnaryFunctionTemplate.H"
#undef _FUNC

end module MAPL_FieldUnaryFunctions


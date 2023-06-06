#include "MAPL_Generic.h"

module MAPL_FieldFunctions
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
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
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Exp
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Log10
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Log
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Sqrt
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Sinh
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Cosh
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Tanh
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Sin
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Cos
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Tan
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Asin
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Acos
#include "FieldFunctionTemplate.H"
#undef _FUNC

#define _FUNC Atan
#include "FieldFunctionTemplate.H"
#undef _FUNC

end module MAPL_FieldFunctions


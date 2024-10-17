#include "MAPL_Generic.h"

module MAPL_FieldBinaryFunctions
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   implicit none
   private

   public :: fieldMin
   public :: fieldMax

contains

#define _FUNC Min
#include "FieldBinaryFunctionTemplate.H"
#undef _FUNC

#define _FUNC Min
#include "FieldBinaryFunctionTemplate.H"
#undef _FUNC

end module MAPL_FieldBinaryFunctions

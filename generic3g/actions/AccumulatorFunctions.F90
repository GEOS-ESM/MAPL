#include "MAPL_Generic.h"

module mapl3g_AccumulatorFunctions.F90
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use mapl3g_AccumulatorConstants

   implicit none
   private

   type :: AccumulatorFunctions
      procedure(FunctionR4R4), pointer :: accumulateR4point => null()
      procedure(FunctionR8R8), pointer :: accumulateR8point => null()
      procedure(FunctionR4), pointer :: clearR4point => null()
      procedure(FunctionR8), pointer :: clearR8point => null()
      logical :: keep_left
      logical :: check_divide
   contains
      procedure :: accumulate
      procedure :: increment
      procedure :: couple
      procedure :: clear
   end type AccumulatorFunctions

   abstract interface
      elemental function FunctionR4R4(left, right) result(val)
         real(kind=ESMF_KIND_R4) :: val
         real(kind=ESMF_KIND_R4), intent(in) :: left, right
      end function FunctionR4R4
      elemental function FunctionR8R8(left, right) result(val)
         real(kind=ESMF_KIND_R8) :: val
         real(kind=ESMF_KIND_R8), intent(in) :: left, right
      end function FunctionR8R8
      elemental function FunctionR4(point) result(val)
         real(kind=ESMF_KIND_R4) :: val
         real(kind=ESMF_KIND_R4), intent(in) :: point
      end function FunctionR4
      elemental function FunctionR8(point) result(val)
         real(kind=ESMF_KIND_R8) :: val
         real(kind=ESMF_KIND_R8), intent(in) :: point
      end function FunctionR8
   end interface

contains

   elemental function AddAccumulateR4R4(left, right) result(val)
      real(kind=ESMF_KIND_R4) :: val
      real(kind=ESMF_KIND_R4), intent(in) :: left, right

      val = left + right

   end function AddAccumulateR4

   elemental function AddAccumulateR8R8(left, right) result(val)
      real(kind=ESMF_KIND_R8) :: val
      real(kind=ESMF_KIND_R8), intent(in) :: left, right

      val = left + right

   end function AddAccumulateR8

   elemental function MinAccumulateR4R4((left, right) result(val)
      real(kind=ESMF_KIND_R4) :: val
      real(kind=ESMF_KIND_R4), intent(in) :: left, right

      val = min(left, right)

   end function MinAccumulateR4R4

   elemental function MinAccumulateR8R8((left, right) result(val)
      real(kind=ESMF_KIND_R8) :: val
      real(kind=ESMF_KIND_R8), intent(in) :: left, right

      val = min(left, right)

   end function MinAccumulateR8R8

   elemental function MaxAccumulateR4R4((left, right) result(val)
      real(kind=ESMF_KIND_R4) :: val
      real(kind=ESMF_KIND_R4), intent(in) :: left, right

      val = max(left, right)

   end function MaxAccumulateR4R4

   elemental function MaxAccumulateR8R8((left, right) result(val)
      real(kind=ESMF_KIND_R8) :: val
      real(kind=ESMF_KIND_R8), intent(in) :: left, right

      val = max(left, right)

   end function MaxAccumulateR8R8

   elemental function ClearZeroR4(point) result(val)
      real(kind=ESMF_KIND_R4) :: val
      real(kind=ESMF_KIND_R4), intent(in) :: point
   end function ClearZeroR4

   elemental function ClearZeroR8(point) result(val)
      real(kind=ESMF_KIND_R8) :: val
      real(kind=ESMF_KIND_R8), intent(in) :: point
   end function ClearZeroR8

   elemental function ClearUndefR4(point) result(val)
      real(kind=ESMF_KIND_R4) :: val
      real(kind=ESMF_KIND_R4), intent(in) :: point
   end function ClearZeroR4

   elemental function ClearUndefR8(point) result(val)
      real(kind=ESMF_KIND_R8) :: val
      real(kind=ESMF_KIND_R8), intent(in) :: point
   end function ClearZeroR8

end module AccumulatorFunctions

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
#include "OperatorFunctionTemplate.H"
#undef _OP
#undef _FUNC

#define _FUNC divide
#define _OP /
#define CHECK_ZEROES
#include "OperatorFunctionTemplate.H"
#undef CHECK_ZEROES
#undef _OP
#undef _FUNC

#define _KEEP_LEFT

#define _FUNC Min
#include "AccumulatorFunctionTemplate.H"
#undef _FUNC

#define _FUNC Min
#include "AccumulatorFunctionTemplate.H"
#undef _FUNC

#undef _KEEP_LEFT

#define _FUNC add
#include "AccumulatorFunctionTemplate.H"
#undef _FUNC

end module mapl3g_AccumulatorFunctions.F90

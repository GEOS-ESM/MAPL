#include "MAPL_Generic.h"
module mapl3g_AccumulatorFunctions.F90
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use mapl3g_AccumulatorConstants

   implicit none
   private

   type, abstract :: FunctionObject
      private
      logical :: keep_left = .FALSE.
      real(kind=ESMF_KIND_R4) :: undefR4
      real(kind=ESMF_KIND_R8) :: undefR8
   end type FunctionObject

   type, extends(FunctionObject) :: UnaryFunctionObject
      private
      procedure(FunctionR4), pointer :: ptrR4
      procedure(FunctionR8), pointer :: ptrR8
   contains
      generic :: eval => evalr4, evalr8
      procedure, nopass :: evalr4
      procedure, nopass :: evalr8
   end type UnaryFunctionObject

   type, extends(FunctionObject) :: BinaryFunctionObject
      private
      logical :: check_zero = .FALSE.
      procedure(FunctionR4R4), pointer :: ptrR4R4
      procedure(FunctionR8R8), pointer :: ptrR8R8
      procedure(FunctionR4R8), pointer :: ptrR4R8
   contains
      generic :: eval => evalr4r4, evalr8r8, evalr4r8
      procedure(FunctionR4R4), nopass :: evalr4r4
      procedure(FunctionR8R8), nopass :: evalr8r8
      procedure(FunctionR4R8), nopass :: evalr4r8
   end type BinaryFunctionObject

   abstract interface
      function FunctionR4R4(lt, rt, rc) result(ft)
         real(kind=ESMF_KIND_R4) :: ft
         real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
         integer, optional, intent(out) :: rc
      end function FunctionR4R4
      function FunctionR8R8(lt, rt, rc) result(ft)
         real(kind=ESMF_KIND_R8) :: ft
         real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
         integer, optional, intent(out) :: rc
      end function FunctionR8R8
      function FunctionR4R8(lt, rt, rc) result(ft)
         real(kind=ESMF_KIND_R4) :: ft
         real(kind=ESMF_KIND_R4), intent(in) :: lt
         real(kind=ESMF_KIND_R8), intent(in) :: rt
         integer, optional, intent(out) :: rc
      end function FunctionR4R8
      function FunctionR4(t, rc) result(ft)
         real(kind=ESMF_KIND_R4) :: ft
         real(kind=ESMF_KIND_R4), intent(in) :: t
         integer, optional, intent(out) :: rc
      end function FunctionR4
      function FunctionR8(lt, rc) result(ft)
         real(kind=ESMF_KIND_R8) :: ft
         real(kind=ESMF_KIND_R8), intent(in) :: t
         integer, optional, intent(out) :: rc
      end function FunctionR8
   end interface

   interface BinaryFunctionObject
      module procedure :: construct_BinaryFunctionObject
   end interface BinaryFunctionObject

   interface UnaryFunctionObject
      module procedure :: construct_UnaryFunctionObject
   end interface UnaryFunctionObject
   
   type :: AccumulatorFunctions
      private
      class(BinaryFunctionObject), pointer :: accumulate_point_function => null()
      class(BinaryFunctionObject), pointer :: couple_point_function => null()
      class(UnaryFunctionObject), pointer :: increment_point_function => null()
      class(UnaryFunctionObject), pointer :: clear_point_function => null()
   contains
      procedure :: accumulate
      procedure :: increment
      procedure :: couple
      procedure :: clear
   end type AccumulatorFunctions

   interface undef
      module procedure :: get_undefined_real
      module procedure :: get_undefined_real64
   end interface undef

contains

   function construct_UnaryFunctionObject(ptrR4, ptrR8) result(fob)
      type(UnaryFunctionObject) :: fob
      procedure(FunctionR4), procedure, pointer, intent(in) :: ptrR4
      procedure(FunctionR8), procedure, pointer, intent(in) :: ptrR8

      fob%ptrR4 => ptrR4
      fob%ptrR8 => ptrR8

   end function construct_UnaryFunctionObject

   function construct_BinaryFunctionObject(ptrR4R4, ptrR8R8, ptrR4R8) result(fob)
      type(BinaryFunctionObject) :: fob
      procedure(FunctionR4R4), procedure, pointer, intent(in) :: ptrR4R4
      procedure(FunctionR8R8), procedure, pointer, intent(in) :: ptrR8R8
      procedure(FunctionR4R8), procedure, pointer, intent(in) :: ptrR4R8

      fob%ptrR4R4 => ptrR4R4
      fob%ptrR4R4 => ptrR4R4
      fob%ptrR4R8 => ptrR4R8

   end function construct_BinaryFunctionObject

   function evalr4(this, t) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      class(UnaryFunctionObject), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: t

      ft = this%ptrR4(t)

   end evalr4

   function evalr8(this, t) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      class(UnaryFunctionObject), intent(in) :: this
      real(kind=ESMF_KIND_R8), intent(in) :: t

      ft = this%ptrR8(t)

   end evalr8

   function evalr4r4(this, lt, rt) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      class(UnaryFunctionObject), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt

      ft = this%ptrR4R4(lt, rt)

   end evalr4r4

   function evalr8r8(this, lt, rt) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      class(UnaryFunctionObject), intent(in) :: this
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt

      ft = this%ptrR8R8(lt, rt)

   end evalr8r8

   function evalr4r8(this, lt, rt) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      class(UnaryFunctionObject), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: lt
      real(kind=ESMF_KIND_R8), intent(in) :: rt

      ft = this%ptrR8R8(lt, rt)

   end evalr8r8
#define _LEFT_ARG lt
#define _RIGHT_ARG rt

#define _LEFTK R4

#define _EXPRESSION 0.0_ESMF_KIND_R4
#define _FUNC ClearZeroR4
#include "AccumulatorFunctionTemplate.H"
#undef _FUNC

#define _EXPRESSION 

#define _RIGHTK R4

#define _EXPRESSION (lt/rt)
#define _FUNC CoupleR4R4
#include "AccumulatorFunctionTemplate.H"
#undef _FUNC
#undef _EXPRESSION

#define _EXPRESSION (lt+rt)
#define _FUNC AddR4R4
#include "AccumulatorFunctionTemplate.H"
#undef _FUNC
#undef _EXPRESSION
#undef _RIGHTK

#define _RIGHTK R8
#define _EXPRESSION (lt/rt)
#define _FUNC CoupleR4R8
#include "AccumulatorFunctionTemplate.H"
#undef _FUNC
#undef _EXPRESSION


   function FunctionR4R4(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function FunctionR4R4
   function FunctionR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function FunctionR8R8
   function FunctionR4R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt
      real(kind=ESMF_KIND_R8), intent(in) :: rt
      integer, optional, intent(out) :: rc
   end function FunctionR4R8
   
   function MaxR4R4(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function MaxR4R4

   function MaxR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function MaxR8R8

   function MaxR4R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt
      real(kind=ESMF_KIND_R8), intent(in) :: rt
      integer, optional, intent(out) :: rc
   end function MaxR4R8

   function MinR4R4(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function MinR4R4

   function MinR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function MinR8R8

   function MinR4R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt
      real(kind=ESMF_KIND_R8), intent(in) :: rt
      integer, optional, intent(out) :: rc
   end function MinR4R8

   function AddR4R4(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function AddR4R4

   function AddR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc
   end function AddR8R8

   function AddR4R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt
      real(kind=ESMF_KIND_R8), intent(in) :: rt
      integer, optional, intent(out) :: rc
   end function AddR4R8

   function construct_AddFunctionObject() result(funobj)
      type(BinaryFunctionObject) :: funobj



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

      val = 0.0_ESMF_KIND_R4

   end function ClearZeroR4

   elemental function ClearZeroR8(point) result(val)
      real(kind=ESMF_KIND_R8) :: val
      real(kind=ESMF_KIND_R8), intent(in) :: point

      val = 0.0_ESMF_KIND_R8

   end function ClearZeroR8

   elemental function ClearUndefR4(point) result(val)
      real(kind=ESMF_KIND_R4) :: val
      real(kind=ESMF_KIND_R4), intent(in) :: point

      val = undef(val)

   end function ClearUndefR4

   elemental function ClearUndefR8(point) result(val)
      real(kind=ESMF_KIND_R8) :: val
      real(kind=ESMF_KIND_R8), intent(in) :: point

      val = undef(val)

   end function ClearUndefR8

   subroutine accumulate(this, field, update, rc)
      class(AccumulatorFunctions), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field, update
      integer, optional, intent(out) :: rc
      integer :: status

      !call 
      
   end subroutine accumulate

   subroutine increment(this, field, rc)
      class(AccumulatorFunctions), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status

      !call 
      
   end subroutine increment

   subroutine couple(this, field, counter, rc)
      class(AccumulatorFunctions), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field, counter
      integer, optional, intent(out) :: rc
      integer :: status

      !call 
      
   end subroutine couple

   subroutine clear(this, rc)
      class(AccumulatorFunctions), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status

      !call 
      
   end subroutine clear

   function get_undefined_real(t) result(u)
      real(kind=ESMF_KIND_R4) :: t
      real(kind=ESMF_KIND_R4), intent(in) :: t
      
      u = MAPL_UNDEFINED_REAL

   end function get_undefined_real

   function get_undefined_real64(t) result(u)
      real(kind=ESMF_KIND_R8) :: t
      real(kind=ESMF_KIND_R8), intent(in) :: t
      
      u = MAPL_UNDEFINED_REAL64

   end function get_undefined_real64
   
end module mapl3g_AccumulatorFunctions

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

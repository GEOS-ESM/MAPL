#include "MAPL_Generic.h"
module mapl3g_AccumulatorFunctions.F90
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use mapl3g_AccumulatorConstants

   implicit none
   private

   type, abstract :: FunctionObject
   contains
   end type FunctionObject

   type, extends(FunctionObject) :: UnaryFunctionObject
      private
      procedure(SubroutineR4), pointer :: ptrR4 => null()
      procedure(SubroutineR8), pointer :: ptrR8 => null()
   contains
      generic :: eval => evalr4, evalr8
      procedure :: evalr4
      procedure :: evalr8
   end type UnaryFunctionObject

   type, extends(FunctionObject) :: BinaryFunctionObject
      private
      procedure(FunctionR4R4), pointer :: ptrR4R4 => null()
      procedure(FunctionR8R8), pointer :: ptrR8R8 => null()
      procedure(FunctionR4R8), pointer :: ptrR4R8 => null()
      logical :: check_zero = .FALSE.
   contains
      generic :: eval => evalr4r4, evalr8r8, evalr4r8
      procedure :: evalr4r4
      procedure :: evalr8r8
      procedure :: evalr4r8
   end type BinaryFunctionObject

   abstract interface
      elemental function FunctionR4R4(lt, rt) result(ft)
         real(kind=ESMF_KIND_R4) :: ft
         real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      end function FunctionR4R4
      elemental function FunctionR8R8(lt, rt) result(ft)
         real(kind=ESMF_KIND_R8) :: ft
         real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      end function FunctionR8R8
      elemental function FunctionR4R8(lt, rt) result(ft)
         real(kind=ESMF_KIND_R4) :: ft
         real(kind=ESMF_KIND_R4), intent(in) :: lt
         real(kind=ESMF_KIND_R8), intent(in) :: rt
      end function FunctionR4R8
      elemental subroutine SubroutineR4(t)
         real(kind=ESMF_KIND_R4), intent(inout) :: t
      end subroutine SubroutineR4
      elemental subroutine SubroutineR8(t)
         real(kind=ESMF_KIND_R8), intent(inout) :: t
      end subroutine SubroutineR8
   end interface

   type :: AccumulatorFunctions
      class(BinaryFunctionObject), pointer :: accumulate_point_function => null()
      class(BinaryFunctionObject), pointer :: couple_point_function => null()
      class(UnaryFunctionObject), pointer :: increment_point_function => null()
      class(UnaryFunctionObject), pointer :: clear_point_function => null()
      logical :: update_on_undef = .TRUE.
   contains
      procedure :: accumulate
      procedure :: increment
      procedure :: couple
      procedure :: clear
   end type AccumulatorFunctions
   
   interface is_undef
      module procedure :: is_undefR4
      module procedure :: is_undefR8
   end interface is_undef

   interface set_undef
      module procedure :: set_undefR4
      module procedure :: set_undefR8
   end interface set_undef

contains

   function MeanAccumulatorFunctions() result(accfunks)
      type(AccumulatorFunctions) :: accfunks
      type(BinaryFunctionObject), pointer :: ptrAccumulate => null()
      type(BinaryFunctionObject), pointer :: ptrCouple => null()
      type(UnaryFunctionObject

      accfunks%accumulate_point_function => AddFunctionObject()
      accfunks%couple_point_function => CoupleFunctionObject()
      accfunks%increment_point_function => IncrementFunctionObject()
      accfunks%clear_point_function => ClearZeroFunctionObject()

   end function MeanAccumulatorFunctions

   function MaxAccumulatorFunctions() result(accfunks)
      type(AccumulatorFunctions) :: accfunks
      type(BinaryFunctionObject), pointer :: ptrAccumulate => null()
      type(BinaryFunctionObject), pointer :: ptrCouple => null()
      type(UnaryFunctionObject

      accfunks%accumulate_point_function => MaxFunctionObject()
      accfunks%clear_point_function => ClearUndefFunctionObject()
      accfunks%update_on_undef = .FALSE.

   end function MaxAccumulatorFunctions

   function MinAccumulatorFunctions() result(accfunks)
      type(AccumulatorFunctions) :: accfunks
      type(BinaryFunctionObject), pointer :: ptrAccumulate => null()
      type(BinaryFunctionObject), pointer :: ptrCouple => null()
      type(UnaryFunctionObject

      accfunks%accumulate_point_function => MinFunctionObject()
      accfunks%clear_point_function => ClearUndefFunctionObject()
      accfunks%update_on_undef = .FALSE.

   end function MinAccumulatorFunctions

   logical function is_undefR4(t) result(lval)
      real(kind=ESMF_KIND_R4), intent(in) :: t

      lval = (t == MAPL_UNDEFINED_REAL)

   end function is_undefR4

   logical function is_undefR8(t) result(lval)
      real(kind=ESMF_KIND_R8), intent(in) :: t

      lval = (t == MAPL_UNDEFINED_REAL64)

   end function is_undefR8
   
   subroutine set_undefR4(t)
      real(kind=ESMF_KIND_R4), intent(inout) :: t

      t = MAPL_UNDEFINED_REAL

   end subroutine set_undefR4

   subroutine set_undefR8(t)
      real(kind=ESMF_KIND_R8), intent(inout) :: t

      t = MAPL_UNDEFINED_REAL64

   end subroutine set_undefR8

   function AddFunctionObject() result(fob)
      type(BinaryFunctionObject) :: fob
      
      fob%ptrR4R4 => AddR4R4
      fob%ptrR8R8 => AddR8R8

   end function AddFunctionObject

   function MaxFunctionObject() result(fob)
      type(BinaryFunctionObject) :: fob
      
      fob%ptrR4R4 => MaxR4R4
      fob%ptrR8R8 => MaxR8R8

   end function MaxFunctionObject
      
   function MinFunctionObject() result(fob)
      type(BinaryFunctionObject) :: fob
      
      fob%ptrR4R4 => MinR4R4
      fob%ptrR8R8 => MinR8R8

   end function MinFunctionObject

   function CoupleFunctionObject() result(fob)
      type(BinaryFunctionObject) :: fob
      
      fob%ptrR4R4 => DivideR4R8
      fob%ptrR8R8 => DivideR8R8

   end function CoupleFunctionObject

   function IncrementFunctionObject() result(fob)
      type(UnaryFunctionObject) :: fob
      
      fob%ptrR4 => IncrementR4
      fob%ptrR8 => IncrementR8

   end function IncrementFunctionObject

   function ClearZeroFunctionObject() result(fob)
      type(UnaryFunctionObject) :: fob
      
      fob%ptrR4 => ClearZeroR4
      fob%ptrR8 => ClearZeroR8

   end function ClearZeroFunctionObject

   function ClearUndefFunctionObject() result(fob)
      type(UnaryFunctionObject) :: fob
      
      fob%ptrR4 => ClearUndefR4
      fob%ptrR8 => ClearUndefR8

   end function ClearUndefFunctionObject

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

   function MaxR4R4(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc

      ft = max(lt, rt)
      _RETURN(_SUCCESS)

   end function MaxR4R4

   function MaxR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc

      ft = max(lt, rt)
      _RETURN(_SUCCESS)

   end function MaxR8R8

   function MinR4R4(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc

      ft = min(lt, rt)
      _RETURN(_SUCCESS)

   end function MinR4R4

   function MinR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc

      ft = min(lt, rt)
      _RETURN(_SUCCESS)

   end function MinR8R8

   function AddR4R4(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc

      ft = lt + rt
      _RETURN(_SUCCESS)

   end function AddR4R4

   function AddR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc

      ft = lt + rt
      _RETURN(_SUCCESS)

   end function AddR8R8

   function DivideR4R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: lt
      real(kind=ESMF_KIND_R8), intent(in) :: rt
      integer, optional, intent(out) :: rc

      ft = real(lt / rt, kind=ESMF_KIND_R4)
      _RETURN(_SUCCESS)

   end function DivideR4R8

   function DivideR8R8(lt, rt, rc) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: lt, rt
      integer, optional, intent(out) :: rc

      ft = lt / rt
      _RETURN(_SUCCESS)

   end function DivideR8R8

   elemental subroutine IncrementR4(t)
      real(kind=ESMF_KIND_R4), intent(inout) :: t

      t = t + 1

   end subroutine IncrementR4

   elemental subroutine IncrementR8(t)
      real(kind=ESMF_KIND_R8), intent(inout) :: t

      t = t + 1

   end subroutine IncrementR8

   elemental subroutine ClearZeroR4(t) result(ft)
      real(kind=ESMF_KIND_R4) :: ft
      real(kind=ESMF_KIND_R4), intent(in) :: t

      ft = 0_ESMF_KIND_R4

   end subroutine ClearZeroR4

   elemental subroutine ClearZeroR8(t) result(ft)
      real(kind=ESMF_KIND_R8) :: ft
      real(kind=ESMF_KIND_R8), intent(in) :: t

      ft = 0_ESMF_KIND_R8

   end subroutine ClearZeroR8

   elemental subroutine ClearUndefR4(t)
      real(kind=ESMF_KIND_R4), intent(inout) :: t

      call set_undef(t)

   end subroutine ClearUndefR4

   elemental subroutine ClearUndefR8(t)
      real(kind=ESMF_KIND_R8), intent(inout) :: t

      call set_undef(t)

   end subroutine ClearUndefR8

   subroutine accumulate(this, field, update, rc)
      class(AccumulatorFunctions), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field, update
      integer, optional, intent(out) :: rc
      integer :: status
      type(BinaryFunctionObject) :: fob => null()
      type(ESMF_TypeKind_Flag) :: tk_left
      type(ESMF_TypeKind_Flag) :: tk_right
      logical :: conformable

      fob => this%accumulate_point_function
      
      conformable = FieldsAreConformable(field, update,_RC)
      _ASSERT(conformable,"Result field is not conformable with update field.")
      call ESMF_FieldGet(field,typekind=tk_left,_RC)
      call ESMF_FieldGet(update,typekind=tk_right,_RC)
      _ASSERT(tk_left == tk_right, "Field typekind does not match update typekind.")

      if(tk_left == ESMF_TypeKind_R4 .and. tk_right == ESMF_TypeKind_R4) then
         call set_r4r4(_RC)
      else if(tk_left == ESMF_TypeKind_R8 .and. tk_right == ESMF_TypeKind_R8) then
         call set_r8r8(_RC)
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)
      
   end subroutine accumulate

   subroutine set_field_binary_same(fob, left, right, rc)
      type(BinaryFunctionObject), intent(in) :: fob
      type(ESMF_Field), intent(inout) :: left, right
      integer, optional, intent(out) :: rc
      integer :: status

      type(ESMF_TypeKind_Flag) :: tk_left
      type(ESMF_TypeKind_Flag) :: tk_right
      logical :: conformable

      conformable = FieldsAreConformable(left, right,_RC)
      _ASSERT(conformable,"Left field is not conformable with right field.")
      call ESMF_FieldGet(left,typekind=tk_left,_RC)
      call ESMF_FieldGet(right,typekind=tk_right,_RC)
      _ASSERT(tk_left == tk_right, "Left typekind does not match right typekind.")

      if(tk_left == ESMF_TypeKind_R4 .and. tk_right == ESMF_TypeKind_R4) then
         call set_r4r4(_RC)
      else if(tk_left == ESMF_TypeKind_R8 .and. tk_right == ESMF_TypeKind_R8) then
         call set_r8r8(_RC)
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)

   contains

      subroutine set_r4r4(rc)
         integer, optional, intent(out) :: rc
         integer :: status
         integer :: kind

         kind = ESMF_KIND_R4
         real(kind=kind), pointer :: ptr_left(:) => null()
         real(kind=kind), pointer :: ptr_right(:) => null()
         real(kind=kind) :: undef(2)

         call GetFieldsUndef([left, right], undef, _RC)
         call assign_fptr(left,ptr_left,_RC)
         call assign_fptr(right,ptr_right,_RC)
         where(
            ptr_out = _OUT_VAL
         elsewhere
            ptr_out = _OUT_VAL_UNDEF
         end where
   end subroutine set_field_binary_same

   subroutine set_r4_r4(
      
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

end module mapl3g_AccumulatorFunctions

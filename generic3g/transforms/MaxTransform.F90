#include "MAPL.h"
#include "accumulator_type_undef.h"

module mapl3g_MaxTransform
   use mapl3g_AccumulatorTransform
   use MAPL_ExceptionHandling
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_FieldPointerUtilities, only: assign_fptr
   use ESMF
   implicit none
   private
   public :: MaxTransform
   public :: construct_MaxTransform

   type, extends(AccumulatorTransform) :: MaxTransform
   contains
      procedure :: accumulate_R4 => max_accumulate_R4
      procedure :: accumulate_R8 => max_accumulate_R8
   end type MaxTransform

contains

   function construct_MaxTransform(typekind) result(acc)
      type(MaxTransform) :: acc
      type(ESMF_TypeKind_Flag), intent(in) :: typekind

      acc%typekind = typekind
      acc%CLEAR_VALUE_R4 = MAPL_UNDEFINED_REAL
      acc%CLEAR_VALUE_R8 = MAPL_UNDEFINED_REAL64

   end function construct_MaxTransform

#define MAX_ACCUMULATOR_
#include "macros_undef.h"
#include "macros.h"
   subroutine max_accumulate_R4(this, update_field, rc)
      class(MaxTransform), intent(inout) :: this
#include "accumulate_template.h"
   end subroutine max_accumulate_R4

#include "macros_undef.h"
#define DP_
#include "macros.h"
   subroutine max_accumulate_R8(this, update_field, rc)
      class(MaxTransform), intent(inout) :: this
#include "accumulate_template.h"
   end subroutine max_accumulate_R8
#undef DP_
#undef MAX_ACCUMULATOR_

end module mapl3g_MaxTransform

#include "MAPL_Generic.h"
module mapl3g_MaxAccumulator
   use mapl3g_AccumulatorAction
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities, only: assign_fptr
   use ESMF
   implicit none
   private
   public :: AccumulatorAction

   type, extends(AccumulatorAction) :: MaxAccumulator
      private
   contains
      procedure :: accumulate_R4 => max_accumulate_R4
   end type MaxAccumulator

   interface MaxAccumulator
      module procedure :: construct_MaxAccumulator
   end interface MaxAccumulator

contains

   function construct_MaxAccumulator() result(acc)
      type(MaxAccumulator) :: acc

      acc%CLEAR_VALUE_R4 = acc%UNDEF_VALUE_R4

   end function construct_MaxAccumulator

   subroutine max_accumulate_R4(this, update_field, rc)
      class(MaxAccumulator), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current(:)
      real(kind=ESMF_KIND_R4), pointer :: latest(:)
      real(kind=ESMF_KIND_R4) :: undef

      undef = this%UNDEF_VALUE_R4
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      where(current == undef)
         current = latest
      elsewhere(latest /= undef)
         current = max(current, latest)
      end where
      _RETURN(_SUCCESS)

   end subroutine max_accumulate_R4

end module mapl3g_MaxAccumulator

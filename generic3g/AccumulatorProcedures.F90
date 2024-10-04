module mapl3g_AccumulatorProcedures

   use mapl3g_Accumulator
   implicit none
   private
   public :: AccumlatorProcedures

   type :: AccumulatorProcedures
      procedure(AccumulateProcedure), pointer :: accumulate => null()
      procedure(CoupleProcedure), pointer :: couple => null()
      procedure(ClearProcedure), pointer :: clear => null()
   end type AccumulatorProcedures

   abstract interface

      subroutine AccumulateProcedure(acc, field_update, rc)
         class(Accumulator), intent(inout) :: acc
         type(ESMF_Field), intent(inout) :: field_update
         integer, optional, intent(out) :: rc
      end subroutine AccumulateProcedure

      subroutine CoupleProcedure(acc, rc)
         class(Accumulator), intent(inout) :: acc
         integer, optional, intent(out) :: rc
      end subroutine CoupleProcedure

      subroutine ClearProcedure(acc, rc)
         class(Accumulator), intent(inout) :: acc
         type(ESMF_Field), intent(inout) :: field_update
         integer, optional, intent(out) :: rc
      end subroutine ClearProcedure

   end interface interface

end module mapl3g_AccumulatorProcedures

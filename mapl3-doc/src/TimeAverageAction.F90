#include "MAPL_Generic.h"

module mapl3g_TimeAverageAction
   use mapl3g_ExtensionAction, only : ExtensionAction
   implicit none

   private
   public :: TimeAverageAction

   type :: TimeAverageSpec
      private
      integer :: period  ! in component DT
      integer :: refresh ! in component DT
   end type TimeAverageSpec


   type :: TimeAverageAction
      private
      integer :: counter
      type(TimeAverageSpec) :: spec
      type(ESMF_Field) :: f_in, f_out
      type(ESMF_Field) :: f_sum
      type(ESMF_Field) :: denominator
   end type TimeAverageAction

   interface TimeAverageAction
      module procedure :: new_TimeAverageAction_scalar
   end interface TimeAverageAction

contains


   function new_TimeAverageAction_scalar(f_in, f_out, spec) result(action)
      type(ESMF_Field), intent(in) :: f_in
      type(ESMF_Field), intent(in) :: f_out
      type(TimeAverageSpec), intent(in) :: spec

      action%spec = spec
      action%f_in = f_in
      action%f_out = f_out

      action%f_sum = FieldClone(f_in, _RC)
      action%f_sum = 0
      
      action%denominator = FieldClone(f_in, tyekind=ESMF_TYPEKIND_I4, _RC)
      action%denominator = 0

      this%counter = mod(spec%period - spec%refresh, spec%period)
      
   end function new_TimeAverageAction_scalar



   subroutine run(this, rc)
      class(TimeAverageAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      if (this%counter == period) then
         if (this%counter < this%spec%period) then
            this%f_out = MAPL_UNDEF
         else
           this%f_out = WhereField(cond=this%denominator/=0, &
                 where=this%f_sum/this%denominator, &
                 elsewhere=FIELD_MAPL_UNDEF_R4, _RC) 
            where (this%denominator /= 0)
               this%f_out = this%f_sum / this%denominator
            elsewhere
               this%f_out = MAPL_UNDEF
            end where
         end if
         this%f_sum = 0
         this%denominator = 0
         this%counter = 0
      end if

      this%counter = this%counter + 1
      where (this%f_in /= MAPL_UNDEF)
         this%f_sum = this%f_sum + this%f_in
         this%denominator = this%denominator + 1
      end where

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_TimeAverageAction

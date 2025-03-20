#include "MAPL_Generic.h"

module mapl3g_TimeAverageTransform
   use mapl3g_ExtensionTransform, only : ExtensionTransform
   implicit none

   private
   public :: TimeAverageTransform

   type :: TimeAverageSpec
      private
      integer :: period  ! in component DT
      integer :: refresh ! in component DT
   end type TimeAverageSpec


   type :: TimeAverageTransform
      private
      integer :: counter
      type(TimeAverageSpec) :: spec
      type(ESMF_Field) :: f_in, f_out
      type(ESMF_Field) :: f_sum
      type(ESMF_Field) :: denominator
   end type TimeAverageTransform

   interface TimeAverageTransform
      module procedure :: new_TimeAverageTransform_scalar
   end interface TimeAverageTransform

contains


   function new_TimeAverageTransform_scalar(f_in, f_out, spec) result(transform)
      type(ESMF_Field), intent(in) :: f_in
      type(ESMF_Field), intent(in) :: f_out
      type(TimeAverageSpec), intent(in) :: spec

      transform%spec = spec
      transform%f_in = f_in
      transform%f_out = f_out

      transform%f_sum = FieldClone(f_in, _RC)
      transform%f_sum = 0
      
      transform%denominator = FieldClone(f_in, tyekind=ESMF_TYPEKIND_I4, _RC)
      transform%denominator = 0

      this%counter = mod(spec%period - spec%refresh, spec%period)
      
   end function new_TimeAverageTransform_scalar



   subroutine run(this, rc)
      class(TimeAverageTransform), intent(inout) :: this
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

end module mapl3g_TimeAverageTransform

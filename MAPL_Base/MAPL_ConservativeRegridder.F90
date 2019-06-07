module MAPL_ConservativeRegridderMod
   use MAPL_AbstractRegridderMod
   use MAPL_TilingRegridderMod
   use ESMF
   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private

   public :: ConservativeRegridder

   type, extends(TilingRegridder) :: ConservativeRegridder
      private
   contains
      procedure, nopass :: add_contribution
   end type ConservativeRegridder
   

contains


   subroutine add_contribution(x_in, weight, x_out, fraction)
      real (kind=REAL32), intent(in) :: x_in
      real (kind=REAL32), intent(in) :: weight
      real (kind=REAL32), intent(inout) :: x_out
      real (kind=REAL32), intent(inout) :: fraction

      x_out = x_out + weight * x_in
      fraction = fraction + weight
      
   end subroutine add_contribution

   
   
end module MAPL_ConservativeRegridderMod

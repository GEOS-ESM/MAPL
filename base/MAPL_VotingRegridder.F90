#include "unused_dummy.H"
module MAPL_VotingRegridderMod
   use MAPL_AbstractRegridderMod
   use MAPL_TilingRegridderMod
   use MAPL_BaseMod, only: MAPL_UNDEF
   use ESMF
   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private

   public :: VotingRegridder

   type, extends(TilingRegridder) :: VotingRegridder
      private
   contains
      procedure, nopass :: init_regrid
      procedure, nopass :: add_contribution
      procedure, nopass :: final_regrid
   end type VotingRegridder
   
contains


   subroutine init_regrid(x_out)
      use, intrinsic :: iso_fortran_env, only: REAL32
      real (kind=REAL32), intent(out) :: x_out(:,:)
      x_out = MAPL_UNDEF
   end subroutine init_regrid
      

   subroutine add_contribution(x_in, weight, x_out, fraction)
      real (kind=REAL32), intent(in) :: x_in
      real (kind=REAL32), intent(in) :: weight
      real (kind=REAL32), intent(inout) :: x_out
      real (kind=REAL32), intent(inout) :: fraction

      _UNUSED_DUMMY(x_in)
      if (weight > fraction) then
         fraction = weight
         x_out = x_in
      end if
      
   end subroutine add_contribution

   
   subroutine final_regrid(x_out, fraction)
      use, intrinsic :: iso_fortran_env, only: REAL32
      real (kind=REAL32), intent(inout) :: x_out(:,:)
      real (kind=REAL32), intent(in) :: fraction(:,:)

      ! Do nothing
      _UNUSED_DUMMY(x_out)
      _UNUSED_DUMMY(fraction)

   end subroutine final_regrid
      
   
end module MAPL_VotingRegridderMod

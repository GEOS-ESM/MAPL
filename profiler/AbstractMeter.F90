module MAPL_AbstractMeter
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: AbstractMeter
   type, abstract :: AbstractMeter
      private
   contains
      ! Override in subclasses for different timing mechanisms
      procedure(i_action), deferred :: start
      procedure(i_action), deferred :: stop
      procedure(i_action), deferred :: reset
      procedure(i_add_cycle), deferred :: add_cycle

      procedure(i_get), deferred :: get_total
      procedure(i_accumulate), deferred :: accumulate

   end type AbstractMeter


   abstract interface

      subroutine i_action(this)
         import AbstractMeter
         class (AbstractMeter), intent(inout) :: this
      end subroutine i_action

      subroutine i_add_cycle(this, increment)
         import AbstractMeter
         import REAL64
         class (AbstractMeter), intent(inout) :: this
         real(kind=REAL64), intent(in) :: increment
      end subroutine i_add_cycle

      function i_get(this) result(val)
         import AbstractMeter
         import REAL64
         real(kind=REAL64) :: val
         class (AbstractMeter), intent(in) :: this
      end function i_get

      subroutine i_accumulate(this, lap)
         import AbstractMeter
         class(AbstractMeter), intent(inout) :: this
         class(AbstractMeter), intent(in) :: lap
      end subroutine i_accumulate

   end interface

end module MAPL_AbstractMeter

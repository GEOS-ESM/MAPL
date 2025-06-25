#include "unused_dummy.H"
#include "MAPL_ErrLog.h"
module MAPL_AbstractMeter
   use MAPL_ErrorHandlingMod
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: AbstractMeter

   logical, save, public :: dist_initialized = .false.
   integer, save, public :: type_dist_struct, type_dist_real64, type_dist_integer
   integer, save, public :: dist_reduce_op

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
      procedure :: finalize

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

   contains

      subroutine finalize(this, rc)
        class(AbstractMeter), intent(in) :: this
        integer, optional, intent(out) :: rc
        integer :: ierror, status

        ierror = 0
        if (dist_initialized) then
           call MPI_type_free(type_dist_struct, ierror)
           _verify(ierror)
           call MPI_type_free(type_dist_real64, ierror)
           _verify(ierror)
           call MPI_type_free(type_dist_integer, ierror)
           _verify(ierror)
           call MPI_Op_free(dist_reduce_op,ierror)
           _verify(ierror)
           dist_initialized = .false.
        endif
        if (present(rc)) rc = ierror
        _unused_dummy(this)
      end subroutine

end module MAPL_AbstractMeter

#include "unused_dummy.H"

module MAPL_MpiTimerGauge
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MPI, only: MPI_Wtime
   use MAPL_AbstractGauge
   implicit none
   private

   public :: MpiTimerGauge

   type, extends(AbstractGauge) :: MpiTimerGauge
      private
   contains
      procedure :: get_measurement
   end type MpiTimerGauge

   interface MpiTimerGauge
      module procedure :: new_MpiTimerGauge
   end interface MpiTimerGauge


contains


   ! Constructor is for convenience - avoids the need of naming a temp
   ! variable when constructing advanced timers.
   function new_MpiTimerGauge() result(gauge)
      type (MpiTimerGauge) :: gauge
      
   end function new_MpiTimerGauge

   function get_measurement(this) result(measurement)
      real(kind=REAL64) :: measurement
      class(MpiTimerGauge), intent(inout) :: this

      _UNUSED_DUMMY(this)
      measurement = MPI_Wtime()

   end function get_measurement

end module MAPL_MpiTimerGauge

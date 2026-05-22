#include "unused_dummy.H"

module mapl_MpiTimerGauge_mod
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MPI, only: MPI_Wtime
   use mapl_AbstractGauge_mod
   implicit none
   private

   public :: MpiTimerGauge

   type, extends(AbstractGauge) :: MpiTimerGauge
      private
   contains
      procedure :: get_measurement
   end type MpiTimerGauge


contains


   function get_measurement(this) result(measurement)
      real(kind=REAL64) :: measurement
      class(MpiTimerGauge), intent(inout) :: this

      _UNUSED_DUMMY(this)
      measurement = MPI_Wtime()

   end function get_measurement

end module mapl_MpiTimerGauge_mod

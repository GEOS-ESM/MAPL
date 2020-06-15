module MAPL_FortranTimerGauge
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use MAPL_AbstractGauge
   implicit none
   private

   public :: FortranTimerGauge

   type, extends(AbstractGauge) :: FortranTimerGauge
      private
      real(kind=REAL64) :: denominator
   contains
      procedure :: get_measurement
   end type FortranTimerGauge

   interface FortranTimerGauge
      module procedure :: new_FortranTimerGauge
   end interface FortranTimerGauge


contains


   function new_FortranTimerGauge() result(gauge)
      type (FortranTimerGauge) :: gauge
      integer(kind=REAL64) :: count_rate

      call system_clock(count_rate=count_rate)
      gauge%denominator = 1._REAL64/count_rate
      
   end function new_FortranTimerGauge


   ! TODO: compute denomintor once during initialization
   function get_measurement(this) result(measurement)
      real(kind=REAL64) :: measurement
      class(FortranTimerGauge), intent(inout) :: this

      integer(kind=INT64) :: tick, rate
      call system_clock(count=tick, count_rate=rate)

      measurement = tick * this%denominator

   end function get_measurement


end module MAPL_FortranTimerGauge

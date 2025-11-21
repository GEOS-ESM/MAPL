module MAPL_NullGauge
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use MAPL_AbstractGauge
   implicit none
   private

   public :: NullGauge

   type, extends(AbstractGauge) :: NullGauge
      private
   contains
      procedure :: get_measurement
   end type NullGauge

   interface NullGauge
      module procedure :: new_NullGauge
   end interface NullGauge


contains


   function new_NullGauge() result(gauge)
      type (NullGauge) :: gauge
      integer(kind=INT64) :: count_rate

   end function new_NullGauge


   ! TODO: compute denomintor once during initialization
   function get_measurement(this) result(measurement)
      real(kind=REAL64) :: measurement
      class(NullGauge), intent(inout) :: this

      measurement = 0

   end function get_measurement


end module MAPL_NullGauge

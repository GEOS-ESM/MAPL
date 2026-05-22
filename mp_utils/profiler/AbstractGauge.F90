module mapl_AbstractGauge_mod
   use, intrinsic :: iso_fortran_env, only: REAL64
   use mapl_AbstractMeter_mod
   implicit none
   private

   public :: AbstractGauge
   type, abstract :: AbstractGauge
      private
   contains
      procedure(i_get_measurement), deferred :: get_measurement
   end type AbstractGauge


   abstract interface

      function i_get_measurement(this) result(measurement)
         import REAL64
         import AbstractGauge
         real(kind=REAL64) :: measurement
         class (AbstractGauge), intent(inout) :: this
      end function i_get_measurement

   end interface


end module mapl_AbstractGauge_mod

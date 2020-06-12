module MAPL_AbstractGauge
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AbstractMeter
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


end module MAPL_AbstractGauge

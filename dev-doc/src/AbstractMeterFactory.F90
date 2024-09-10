module MAPL_AbstractMeterFactory
   use MAPL_AbstractMeter
   implicit none
   private

   public :: AbstractMeterFactory

   type, abstract :: AbstractMeterFactory
   contains
      procedure(i_make_meter), deferred :: make_meter
   end type AbstractMeterFactory

   abstract interface
      function i_make_meter(this) result(meter)
         import AbstractMeterFactory
         import AbstractMeter
         class(AbstractMeter), allocatable :: meter
         class(AbstractMeterFactory), intent(in) :: this
      end function i_make_meter
   end interface

end module MAPL_AbstractMeterFactory
  

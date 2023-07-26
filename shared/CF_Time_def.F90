module CF_Time_def_mod

   implicit none

   private

   public :: CF_Time

   type, abstract :: CF_Time
      private
      character(len=:), allocatable :: units_
   contains
      procedure, public, pass(this) :: units => get_cf_time_units
      procedure, deferred, public, pass(this) :: duration
   end type CF_Time

contains

   function get_cf_time_units(this) result(units)
      class(CF_Time), intent(in) :: this
      character(len=:), allocatable :: units

      units = this % units_

   end function get_cf_time_units

end module CF_Time_def_mod

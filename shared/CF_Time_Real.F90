module CF_Time_Real_mod

   use CF_Time_def_mod

   implicit none

   private

   public :: CF_Time_Real
   public :: construct_CF_Time_Real

   type, extends(CF_Time) :: CF_Time_Real
      private
      real :: duration_
   contains
      procedure, public, pass(this) :: duration => get_cf_time_duration_real
   end type CF_Time_Integer

contains

   function construct_CF_Time_Real(duration, units) result(cft)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units
      type(CF_Time_Integer) :: cft

      cft % duration_ = duration
      cft % units_ = units

   end function construct_CF_Time_Real

   real function get_cf_time_duration_real(this)
      class(CF_Time_Integer), intent(in) :: this

      get_cf_time_duration_integer = this % duration_

   end function get_cf_time_duration_real

end module CF_Time_Real_mod

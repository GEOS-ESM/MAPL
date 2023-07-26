module CF_Time_Integer_mod

   use CF_Time_def_mod

   implicit none

   private

   public :: CF_Time_Integer
   public :: construct_CF_Time_Integer

   type, extends(CF_Time) :: CF_Time_Integer
      private
      integer :: duration_
   contains
      procedure, public, pass(this) :: duration => get_cf_time_duration_integer
   end type CF_Time_Integer

contains

   function construct_CF_Time_Integer(duration, units) result(cft)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units
      type(CF_Time_Integer) :: cft

      cft % duration_ = duration
      cft % units_ = units

   end function construct_CF_Time_Integer

   integer function get_cf_time_duration_integer(this)
      class(CF_Time_Integer), intent(in) :: this

      get_cf_time_duration_integer = this % duration_

   end function get_cf_time_duration_integer

end module CF_Time_Integer_mod

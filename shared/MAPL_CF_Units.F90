module MAPL_CF_Units

   use CF_Time_mod

   implicit none

   private

   public :: convert_cf_time_to_iso8601, convert_iso8601_to_cf_time_real, convert_iso8601_to_cf_time_integer

   interface convert_cf_time_to_iso8601
      module procedure :: convert_cf_time_to_iso8601_integer
      module procedure :: convert_cf_time_to_iso8601_real
      module procedure :: convert_cf_time_to_iso8601_dt
   end interface convert_cf_time_to_iso8601
   
   character(len=*), parameter :: FRAC_DELIM = '.'
   character(len=*), parameter :: TIME_DELIM = ':'
   character(len=*), parameter :: DATE_DELIM = '-'
   character(len=*), parameter :: DT_DELIM = ' T'

contains

   subroutine convert_cf_time_to_iso8601_integer(duration, units, isotime, rc)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units
      character(len=:), allocatable, intent(out) :: isotime
      integer, optional, intent(out) :: rc
      integer :: status 

   end subroutine convert_cf_time_to_iso8601_integer

   subroutine convert_cf_time_to_iso8601_real(duration, units, isotime, rc)
      real, intent(in) :: duration
      character(len=*), intent(in) :: units
      character(len=:), allocatable, intent(out) :: isotime
      real, optional, intent(out) :: rc
      integer :: status
   end subroutine convert_cf_time_to_iso8601_real

   subroutine convert_cf_time_to_iso8601_dt(cftime, isotime, rc)
      class(CF_Time), intent(in) :: cftime
      character(len=:), allocatable, intent(out) :: isotime
      real, optional, intent(out) :: rc
      integer :: status
      
      call convert_cf_time_to_iso8601(cftime % duration(), cftime % units(), isotime, _RC)

   end subroutine convert_cf_time_to_iso8601_dt
   
   subroutine convert_iso8601_to_cf_time(isotime, cftime, rc)
      character(len=*), intent(in) :: isotime
      type(CF_Time_Real), intent(out) :: cftime
      integer, optional, intent(out) :: rc 
      character(len=4) :: year
      character(len=2) :: month
      character(len=2) :: day
      character(len=2) :: hour
      character(len=2) :: minute
      character(len=2) :: second
      character(len=:), allocatable :: second_fraction


   end subroutine convert_iso8601_to_cf_time

   subroutine convert_iso8601_to_cf_time_integer(isotime, cftime, rc)
      character(len=*), intent(in) :: isotime
      type(CF_Time_Integer), intent(out) :: cftime
      integer, optional, intent(out) :: rc 
      class(CF_Time_Real) :: cftime_real
      integer :: status 

      call convert_iso8601_to_cf_time(isotime, cftime_real, _RC)
      cftime = CF_Time(integer(cftime_real % duration()), cftime_real % units(), _RC)

   end subroutine convert_iso8601_to_cf_time_integer

   function make_CF_Time_reference(parts, zero_pad, cft_ref) result(cft_ref)
      character(len=*), intent(in) :: year
      character(len=*), intent(in) :: month
      character(len=*), intent(in) :: day
      character(len=*), intent(in) :: hour
      character(len=*), intent(in) :: minute
      character(len=*), intent(in) :: second
      character(len=*), intent(in) :: second_fraction
      logical, optional, intent(in) :: zero_pad
      logical, optional, intent(in) :: use_t
      character(len=:), allocatable :: cft_ref
   end function make_CF_Time_reference

end module MAPL_CF_Units

module CF_Time_def_mod

   implicit none

   private

   public :: CF_Time

module MAPL_TimeUtilsMod

   implicit none
   private

   public :: is_valid_date
   public :: is_valid_time
   public :: is_valid_datetime

contains

   logical function is_valid_date(date) result(is_valid)

      ! Function to validate what MAPL expects for a valid date as
      ! passed to, say, History. 

      integer, intent(in) :: date

      integer :: year, month, day
      logical :: is_leap_year

      year  = date/10000
      month = mod(date,10000)/100
      day   = mod(date,100)

      is_leap_year = mod(year,4) == 0 .and. ( mod(year,100) /= 0  .or. mod(year,400) == 0 )

      is_valid = .true.

      ! Obvious cases
      if (date < 0) then
         is_valid = .false.
      else if (month == 0) then
         is_valid = .false.
      else if (month > 12) then
         is_valid = .false.
      else if (day == 0) then
         is_valid = .false.
      end if

      select case (month)
      ! 30 day months
      case (4,6,9,11)
         if (day > 30) is_valid = .false.
      ! February
      case (2)
         if (is_leap_year) then
            if (day > 29) is_valid = .false.
         else
            if (day > 28) is_valid = .false.
         end if
      ! 31 day months
      case default
         if (day > 31) is_valid = .false.
      end select

   end function is_valid_date

   logical function is_valid_time(time) result(is_valid)

      ! Function to validate what MAPL expects for a valid time as
      ! passed to, say, History. In this case it is a 6-digit integer
      ! that ranges from 000000 to 240000. 

      integer, intent(in) :: time

      integer :: hours, minutes, seconds

      hours   = time/10000
      minutes = mod(time,10000)/100
      seconds = mod(time,100)

      is_valid = .true.

      if (time < 0) then
         is_valid = .false.
      else if (time > 240000) then
         is_valid = .false.
      else if (hours > 24) then
         is_valid = .false.
      else if (minutes > 59) then
         is_valid = .false.
      else if (seconds > 59) then
         is_valid = .false.
      end if

   end function is_valid_time

   logical function is_valid_datetime(datetime) result(is_valid)

      ! Function to validate a datetime array

      integer, intent(in) :: datetime(2)

      is_valid = is_valid_date(datetime(1)) .and. is_valid_time(datetime(2))

   end function is_valid_datetime

end module MAPL_TimeUtilsMod

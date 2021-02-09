module MAPL_DateValidateMod

   implicit none
   private

   public :: is_valid_date

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

end module MAPL_DateValidateMod

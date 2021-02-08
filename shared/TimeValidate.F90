module MAPL_TimeValidateMod

   implicit none
   private

   public :: is_valid_time

contains

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

end module MAPL_TimeValidateMod

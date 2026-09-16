module mapl_Sleep_mod

use, intrinsic :: iso_c_binding, only: C_INT
use, intrinsic :: iso_fortran_env, only: REAL64,INT64
implicit none
private

public MAPL_Sleep
public MAPL_PassiveSleep

interface
   function c_usleep(microseconds) bind(C, name='usleep') result(status)
      import C_INT
      integer(C_INT), value :: microseconds
      integer(C_INT) :: status
   end function c_usleep
end interface

contains

! wait time in seconds
subroutine MAPL_Sleep(wait_time)
real, intent(in) :: wait_time

integer(kind=INT64) :: s1,s2,count_max,count_rate,delta
real(kind=REAL64) :: seconds_elapsed

call system_clock(count=s1,count_rate=count_rate,count_max=count_max)

do 

   call system_clock(count=s2)
   delta = s2-s1
   if (delta < 0) delta= s2 + (count_max - mod(s1,count_max))
   seconds_elapsed = dble(delta)/dble(count_rate)
   if (seconds_elapsed > wait_time) exit

enddo

end subroutine

subroutine MAPL_PassiveSleep(wait_time)
real, intent(in) :: wait_time

integer(C_INT) :: remaining_microseconds
integer(C_INT), parameter :: MAX_SLEEP_MICROSECONDS = 1000000000_C_INT

remaining_microseconds = int(max(0.0, wait_time) * 1000000.0, C_INT)
do while (remaining_microseconds > 0)
   if (c_usleep(min(remaining_microseconds, MAX_SLEEP_MICROSECONDS)) /= 0) return
   remaining_microseconds = remaining_microseconds - MAX_SLEEP_MICROSECONDS
end do

end subroutine MAPL_PassiveSleep
end module mapl_Sleep_mod

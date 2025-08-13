module MAPL_SleepMod

use, intrinsic :: iso_fortran_env, only: REAL64,INT64
implicit none
private

public MAPL_Sleep

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
end module MAPL_SleepMod

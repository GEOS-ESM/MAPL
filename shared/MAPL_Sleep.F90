module MAPL_SleepMod

implicit none
private

public MAPL_Sleep

contains

! wait time in milliseconds
subroutine MAPL_Sleep(wait_time)
integer, intent(in) :: wait_time

integer :: t(8)

integer :: current_time, previous_time, start_of_day
integer :: total_accumulation, temp_accumulation, previous_days_accumulated

call date_and_time(values=t)

current_time = (t(5)*3600+t(6)*60+t(7))*1000+t(8)
start_of_day = current_time
previous_time = 0
previous_days_accumulated = 0
total_accumulation = 0

do

   call date_and_time(values=t)
   current_time = (t(5)*3600+t(6)*60+t(7))*1000+t(8)
   temp_accumulation = current_time - start_of_day
   if (current_time < previous_time) then
      start_of_day = 0
      previous_days_accumulated = previous_days_accumulated + previous_time
      temp_accumulation = current_time - start_of_day
   end if
   total_accumulation = temp_accumulation + previous_days_accumulated
   previous_time = temp_accumulation
   if ( total_accumulation > wait_time ) exit

enddo

end subroutine
end module MAPL_SleepMod

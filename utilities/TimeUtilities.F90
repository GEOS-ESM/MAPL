#include "MAPL.h"

module mapl3g_TimeUtilities

   implicit none
   private

   public PackTime
   public PackDateTime
   public UnpackTime
   public UnpackDateTime

contains

   subroutine UnpackTime(time, iyy, imm, idd)
      integer, intent(in) :: time
      integer, intent(out) :: iyy
      integer, intent(out) :: imm
      integer, intent(out) :: idd
      iyy = time/10000
      imm = mod(time/100,100)
      idd = mod(time,100)
   end subroutine UnpackTime

   subroutine PackTime(time, iyy, imm, idd)
      integer, intent(out) :: time
      integer, intent(in) :: iyy
      integer, intent(in) :: imm
      integer, intent(in) :: idd
      time = iyy*10000 + imm*100 + idd
   end subroutine PackTime

   subroutine PackDateTime(date_time, yy, mm, dd, h, m, s)
      integer, intent(in) :: yy, mm, dd, h, m, s
      integer, intent(out) :: date_time(:)

      date_time(1) = (10000 * yy) + (100 * mm) + dd
      date_time(2) = (10000 * h) + (100 * m) + s
   end subroutine PackDateTime

   subroutine UnpackDateTime(date_time, yy, mm, dd, h, m, s)
      integer, intent(in) :: date_time(:)
      integer, intent(out) :: yy, mm, dd, h, m, s

      yy = date_time(1) / 10000
      mm = mod(date_time(1), 10000) / 100
      dd = mod(date_time(1), 100)
      h = date_time(2) / 10000
      m = mod(date_time(2), 10000) / 100
      s = mod(date_time(2), 100)
   end subroutine UnpackDateTime

end module mapl3g_TimeUtilities

#include "MAPL.h"

module mapl_TimeUtilities_mod

   implicit none
   private

   public PackDate
   public PackDateTime
   public UnpackDate

contains

   subroutine UnpackDate(time, iyy, imm, idd)
      integer, intent(in) :: time
      integer, intent(out) :: iyy
      integer, intent(out) :: imm
      integer, intent(out) :: idd
      iyy = time/10000
      imm = mod(time/100,100)
      idd = mod(time,100)
   end subroutine UnpackDate

   subroutine PackDate(time, iyy, imm, idd)
      integer, intent(out) :: time
      integer, intent(in) :: iyy
      integer, intent(in) :: imm
      integer, intent(in) :: idd
      time = iyy*10000 + imm*100 + idd
   end subroutine PackDate

   subroutine PackDateTime(date_time, yy, mm, dd, h, m, s)
      integer, intent(in) :: yy, mm, dd, h, m, s
      integer, intent(out) :: date_time(:)

      date_time(1) = (10000 * yy) + (100 * mm) + dd
      date_time(2) = (10000 * h) + (100 * m) + s
   end subroutine PackDateTime

end module mapl_TimeUtilities_mod

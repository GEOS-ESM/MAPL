#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_ExtDataUtilities
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public in_range

   contains

   logical function in_range(t1, t2, t0, open_end)
      type(ESMF_Time), intent(in) :: t1
      type(ESMF_Time), intent(in) :: t2
      type(ESMF_Time), intent(in) :: t0
      logical, optional, intent(in) :: open_end

      logical usable_open_end
      usable_open_end=.false.
      if (present(open_end)) usable_open_end = open_end
      if (usable_open_end) then
         in_range = (t0 >= t1) .and. (t0 <= t2)
      else
         in_range = (t0 >= t1) .and. (t0 < t2)
      end if
   end function in_range
end module

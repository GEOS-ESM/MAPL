      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=KIND_), pointer :: current(:)
      real(kind=KIND_), pointer :: latest(:)
#if defined(MEAN_ACCUMULATOR_)
      integer(kind=COUNTER_KIND), pointer :: counter(:)

      counter => null()
      call assign_fptr(this%counter_field, counter, _RC)
#endif
      current => null()
      latest => null()
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
#if defined(MEAN_ACCUMULATOR_)
      where(latest /= UNDEF_)
        current = current + latest
        counter = counter + 1_COUNTER_KIND
      end where
#elif defined(MAXMIN_)
      where(current == UNDEF_)
         current = latest
      elsewhere(latest /= UNDEF_)
#  if defined(MAX_ACCUMULATOR_)
         current = max(current, latest)
#  else
         current = min(current, latest)
#  endif
      end where
#else
      where(current /= UNDEF_ .and. latest /= UNDEF_)
        current = current + latest
      elsewhere(latest == UNDEF_)
        current = UNDEF_
      end where
#endif
      _RETURN(_SUCCESS)
#include "macros_undef.h"
! vim: ft=fortran

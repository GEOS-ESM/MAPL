      where(latest /= UNDEF_)
        current = current + latest
        counter = counter + 1_COUNTER_KIND
      end where
      _RETURN(_SUCCESS)
! vim: ft=fortran

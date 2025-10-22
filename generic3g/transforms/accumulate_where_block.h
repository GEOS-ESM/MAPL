      where(current /= UNDEF .and. latest /= UNDEF)
        current = current + latest
      elsewhere(latest == UNDEF)
        current = UNDEF
      end where
      _RETURN(_SUCCESS)
! vim: ft=fortran

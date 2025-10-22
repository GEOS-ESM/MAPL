      where(current /= UNDEF_ .and. latest /= UNDEF_)
        current = current + latest
      elsewhere(latest == UNDEF_)
        current = UNDEF_
      end where
      _RETURN(_SUCCESS)
! vim: ft=fortran

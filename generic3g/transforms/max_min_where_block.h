      where(current == UNDEF_)
         current = latest
      elsewhere(latest /= UNDEF_)
         current = FUNC_ (current, latest)
      end where
      _RETURN(_SUCCESS)
! vim: ft=fortran

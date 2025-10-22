      where(current == UNDEF)
         current = latest
      elsewhere(latest /= UNDEF)
         current = FUNC_(current, latest)
      end where
      _RETURN(_SUCCESS)
! vim: ft=fortran

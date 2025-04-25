subroutine foo(x)
  use iso_c_binding
  real, target :: x(*)
  type (C_PTR) :: loc
  loc = c_loc(x(1))
end subroutine foo

program main
end program main


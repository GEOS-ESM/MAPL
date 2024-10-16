module A
  implicit none (type, external)

  generic s => s1
contains

  subroutine s1(x)
    real, intent(inout) :: x

    x = x + 1
  end subroutine s1
end module A

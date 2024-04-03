! vim:ft=fortran
      type(HConfigParams), intent(inout) :: params
      character(len=:), allocatable, optional, intent(out) :: valuestring
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
      character(len=:), allocatable :: valuestring_
      character(len=ESMF_MAXSTR) :: buffer
      character(len=:), allocatable :: fmtstr

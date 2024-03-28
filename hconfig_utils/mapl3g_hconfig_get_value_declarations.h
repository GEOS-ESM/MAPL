! vim:ft=fortran
      type(HConfigParams), intent(inout) :: params
      character(len=:), allocatable, optional, intent(out) :: valuestring_out
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
      character(len=:), allocatable :: valuestring
      character(len=*), parameter :: fmtstr = '(' // FMT_ //')'
      character(len=ESMF_MAXSTR) :: buffer

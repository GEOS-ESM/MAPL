      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=KIND_), pointer :: current(:)
      real(kind=KIND_), pointer :: latest(:)
#if defined(USE_COUNTER_)
      integer(kind=COUNTER_KIND), pointer :: counter(:)

      counter => null()
      call assign_fptr(this%counter_field, counter, _RC)
#endif
      current => null()
      latest => null()
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
! vim: ft=fortran

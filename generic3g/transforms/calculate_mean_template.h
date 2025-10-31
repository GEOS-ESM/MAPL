      class(MeanTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=KIND_), pointer :: current_ptr(:)
      integer(kind=COUNTER_KIND), pointer :: counter(:)

      current_ptr => null()
      counter => null()
      call assign_fptr(this%accumulation_field, current_ptr, _RC)
      call assign_fptr(this%counter_field, counter, _RC)
      where(counter /= 0)
         current_ptr = current_ptr / counter
      elsewhere
         current_ptr = UNDEF_
      end where
      _RETURN(_SUCCESS)
! vim: ft=fortran

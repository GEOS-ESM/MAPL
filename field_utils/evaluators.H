subroutine evaluate_R4I4(field_out, field_left, field_right, has_undef, rc)
    type(ESMF_Field), intent(inout) :: field_out
    type(ESMF_Field), intent(in) :: field_left
    type(ESMF_Field), intent(in) :: field_right
    logical, intent(in) :: has_undef
    integer, optional, intent(out) :: rc
    integer :: status

    real(kind=ESMF_KIND_R4) :: left_undef, out_undef
    integer(kind=ESMF_KIND_I4) :: right_undef
    real(kind=ESMF_KIND_R4), pointer :: left_ptr(:), out_ptr(:)
    integer(kind=ESMF_KIND_I4), pointer :: right_ptr(:)

    if(has_undef) then
       where(left_ptr /= left_undef .and. right_ptr /= right_undef)
          out_ptr = left_ptr _OP right_ptr


(x, fptr, rc)
type(ESMF_Field), intent(inout) :: x
TYPE_(kind=KIND_), pointer, intent(out) :: fptr(:)
integer, optional, intent(out) :: rc

! local declarations
type(c_ptr) :: cptr
integer(ESMF_KIND_I8), allocatable :: fp_shape(:)
integer(ESMF_KIND_I8) :: local_size
integer :: status

local_size = FieldGetLocalSize(x, _RC)
fp_shape = [ local_size ]
call FieldGetCptr(x, cptr, _RC)
call c_f_pointer(cptr, fptr, fp_shape)

_RETURN(_SUCCESS)

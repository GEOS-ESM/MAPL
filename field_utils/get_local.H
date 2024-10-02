integer function get_local_count(field, rc) result(local_count)
   type(ESMF_Field), intent(in) :: field
   integer, optional, intent(out) :: rc
   integer :: status
   TYPE_, pointer :: array_ptr DIMS_

   call ESMF_FieldGet(field,0,farrayptr=array_ptr, RC)
   local_count = shape(array_ptr)
   _RETURN(_SUCCESS)
end function get_local_count

integer function int_tkr(tk, rank) result(n)
   type(ESMF_TypeKind_Flag), intent(in) :: tk
   integer, intent(in) :: rank
   integer, parameter :: TK_FACTOR = 10
   type(ESMF_TypeKind_Flag), parameter :: TKS = &
      & [ESMF_TypeKind_R4, ESMF_TypeKind_R8, ESMF_TypeKind_I4, ESMF_TypeKind_I8]

   do n=1, size(TKS)
      if(TKS(n) == tk) return
   end do

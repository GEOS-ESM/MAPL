logical function FUNCNAME_(val) result(isin)
    type(TYPE_), intent(in) :: val
    type(TYPE_), parameter :: array(*) = SET_
    integer :: i
    
    do i = 1, size(array)
        isin = val == array(i)
        if(isin) exit
    end do

end function FUNCNAME_
#undef FUNCNAME_
#undef TYPE_
#undef SET_

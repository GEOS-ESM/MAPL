module MAPL_TimeDependentMask

  type :: TimeDependentMask
     logical, allocatable :: mask(:,:)    ! for CS and LL
   contains
     procedure :: get_mask
  end type TimeDependentMask

  subroutine get_mask(this)
    type(TimeDependentMask), intent(inout) :: this
    
    this%mask = 1
  end subroutine get_mask
end module MAPL_TimeDependentMask
    


   function get_array(info_in, rc) result(array)
      type(ESMF_Info), intent(in) :: info_in
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: PREFIX = 'MAPL/'
      integer :: status
      integer :: num_ungridded
      integer :: i, ios
      character(len=32) :: stri
      type(UngriddedDimInfo), allocatable :: array(:)

      call ESMF_InfoGet(info_in, PREFIX // 'num_ungridded', num_ungridded, _RC)
      _ASSERT(num_ungridded >= 0, 'num_ungridded must be nonnegative.')
      allocate(array(num_ungridded))
      if(num_ungridded == 0) then
         _RETURN(_SUCCESS)
      end if
      do i= 1, num_ungridded
         write(stri, fmt='(I0)', iostat=ios) i
         _ASSERT(ios == 0, 'failed to create ith ungridded dim index string')
         array(i) = UngriddedDimInfo(info_in, PREFIX // 'dim_' // trim(adjustl(stri)) // '/')
      end do

      _RETURN(_SUCCESS)

   end function get_array

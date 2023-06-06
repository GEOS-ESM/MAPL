module test_MAPL_NetCDF_helpers

   use ESMF

   implicit none

   integer, parameter :: SUCCESS = 0

contains
!===============================================================================
! HELPERS
!===============================================================================
   function make_datetime_string(yy, mm, dd, h, m, s) result(datetime_string)
      integer, intent(in) :: yy, mm, dd, h, m, s
      character(len=32) :: datetime_string
      character(len=*), parameter :: fmt_string = '(I4.4, "-", I2.2, "-", I2.2, 1X, I2.2, ":", I2.2, ":", I2.2)'
      integer :: iostat_

      write(datetime_string, fmt=fmt_string, iostat=iostat_) yy, mm, dd, h, m, s
      if(iostat_ == SUCCESS) return
      datetime_string = ''

   end function make_datetime_string

   function make_units_string(units, preposition, yy, mm, dd, h, m, s) result(units_string)
      character(len=*), intent(in) :: units
      character(len=*), intent(in) :: preposition
      integer, intent(in) :: yy, mm, dd, h, m, s
      character(len=132) :: units_string
      character(len=:), allocatable :: datetime_string
      character(len=*), parameter :: SPACE = ' '

      units_string = ''
      datetime_string = make_datetime_string(yy, mm, dd, h, m, s)
      if(len_trim(datetime_string) == 0) return
      units_string = trim(units) // SPACE // trim(preposition) // SPACE // datetime_string

   end function make_units_string

   logical function rational_equals(na, nb)
      integer, intent(in) :: na(2)
      integer, intent(in) :: nb(2)

      rational_equals = ( na(1) * nb(2) == na(2) * nb(1) )

   end function rational_equals

   function ESMF_Times_Equal(timeu, timev) result(tval)
      type(ESMF_Time), intent(in) :: timeu, timev
      logical :: tval
      integer :: uyy, umm, udd, uh, um, us, usN, usD
      integer :: vyy, vmm, vdd, vh, vm, vs, vsN, vsD
      integer :: status

      tval = .FALSE.
      call ESMF_TimeGet(timeu, yy=uyy, mm=umm, dd=udd, h=uh, m=um, d=us, sN=usN, sD=usD, rc = status)
      if(status /= SUCCESS) return
      call ESMF_TimeGet(timev, yy=vyy, mm=vmm, dd=vdd, h=vh, m=vm, d=vs, sN=vsN, sD=vsD, rc = status)
      if(status /= SUCCESS) return

      tval = ( (uyy == vyy) .and. (umm == vmm) .and. (udd == vdd) &
         .and. (uh == vh) .and. (um == vm) .and. (us == vs) &
         .and. rational_equals([usN, usD], [vsN, vsD]) )

   end function ESMF_Times_Equal

end module test_MAPL_NetCDF_helpers

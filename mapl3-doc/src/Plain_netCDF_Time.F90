!-------------------------------------------------------------------
! Note: use for OSSE project
!   File to be replaced by more systematic implementations.
!   It contains codes for time conversion and bisect.
!-------------------------------------------------------------------

#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

!
!>
!### MODULE: `Plain_netCDF_Time`
!
! Author: GMAO SI-Team
!
module Plain_netCDF_Time
  use MAPL_KeywordEnforcerMod
  use MAPL_ExceptionHandling
  use MAPL_ShmemMod
  use MAPL_ErrorHandlingMod
  use MAPL_Constants
  use ESMF
  use pfio_NetCDF_Supplement
  !   use MAPL_CommsMod
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use, intrinsic :: iso_c_binding, only: C_INT
  implicit none
  public

  interface convert_time_nc2esmf
     procedure :: time_nc_int_2_esmf
  end interface convert_time_nc2esmf

  interface convert_time_esmf2nc
     procedure :: time_esmf_2_nc_int
  end interface convert_time_esmf2nc

  interface get_v2d_netcdf
     procedure ::  get_v2d_netcdf_R4
     procedure ::  get_v2d_netcdf_R8
  end interface get_v2d_netcdf

  interface parse_timeunit
     procedure :: parse_timeunit_i4
     procedure :: parse_timeunit_i8
  end interface parse_timeunit

  interface hms_2_s
     procedure :: hms_2_s
  end interface hms_2_s

  interface bisect
     procedure :: bisect_find_LB_R8_I8
  end interface bisect

contains

  logical function is_success(c)
     integer, intent(in) :: c

     is_success = (c == _SUCCESS)

  end function is_success

  subroutine get_ncfile_dimension(filename, nlon, nlat, tdim, key_lon, key_lat, key_time, rc)
    use netcdf
    implicit none
    character(len=*), intent(in) :: filename
    integer, optional,intent(out) :: nlat, nlon, tdim
    character(len=*), optional, intent(in)  :: key_lon, key_lat, key_time
    integer, optional, intent(out) :: rc
    integer :: ncid , dimid
    integer :: status
    character(len=ESMF_MAXSTR) :: lon_name, lat_name, time_name

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    if(present(key_lon)) then
       lon_name=trim(key_lon)
       call check_nc_status(nf90_inq_dimid(ncid, trim(lon_name), dimid), _RC)
       call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=nlon), _RC)
    endif

    if(present(key_lat)) then
       lat_name=trim(key_lat)
       call check_nc_status(nf90_inq_dimid(ncid, trim(lat_name), dimid), _RC)
       call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=nlat), _RC)
    endif

    if(present(key_time)) then
       time_name=trim(key_time)
       call check_nc_status(nf90_inq_dimid(ncid, trim(time_name), dimid), _RC)
       call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=tdim), _RC)
    endif
    call check_nc_status(nf90_close(ncid), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_ncfile_dimension


  subroutine get_attribute_from_group(filename, group_name, var_name, attr_name, attr, rc)
    use netcdf
    use pfio_NetCDF_Supplement
    implicit none
    character(len=*), intent(in) :: filename, group_name, var_name, attr_name
    character(len=*), intent(INOUT) :: attr
    integer, optional, intent(out) :: rc
    integer :: ncid, varid, ncid2
    integer :: status
    integer :: len, i, j, k
    integer :: xtype
    character(len=:), allocatable :: str
    integer(kind=C_INT) :: c_ncid, c_varid
    character(len=100) :: str2

    call check_nc_status(nf90_open(fileName, NF90_NOWRITE, ncid2), _RC)
    if (group_name/='') then
       call check_nc_status(nf90_inq_ncid(ncid2, group_name, ncid), _RC)
    else
       ncid = ncid2
    end if
    call check_nc_status(nf90_inq_varid(ncid, var_name, varid), _RC)
    call check_nc_status(nf90_inquire_attribute(ncid, varid, attr_name, xtype, len=len), _RC)
    c_ncid= ncid
    c_varid= varid
    select case (xtype)
    case(NF90_STRING)
       _ASSERT(is_success(pfio_get_att_string(c_ncid, c_varid, attr_name, str)), 'Error return from pfio_get_att_string')
    case(NF90_CHAR)
       allocate(character(len=len) :: str)
       call check_nc_status(nf90_get_att(ncid, varid, trim(attr_name), str), _RC)
    case default
       _FAIL('code works only with string attribute')
    end select
    i=index(str, 'since')
    ! get rid of T in 1970-01-01T00:00:0
    str2=str(i+6:i+24)
    j=index(str2, 'T')
    if(j>1) then
       k=len_trim(str2)
       str2=str2(1:j-1)//' '//str2(j+1:k)
    endif
    attr = str(1:i+5)//trim(str2)
    deallocate(str)
    call check_nc_status(nf90_close(ncid2), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_attribute_from_group


  subroutine get_v2d_netcdf_R4(filename, name, array, Xdim, Ydim, rc)
    use netcdf
    implicit none
    character(len=*), intent(in) :: name, filename
    integer, intent(in) :: Xdim, Ydim
    real, dimension(Xdim,Ydim), intent(out) :: array
    integer, optional, intent(out) :: rc
    integer :: status
    integer :: ncid, varid
    real    :: scale_factor, add_offset
    integer :: iret

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    call check_nc_status(nf90_inq_varid(ncid, name, varid), _RC)
    call check_nc_status(nf90_get_var(ncid, varid, array), _RC)

    iret = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    if(iret .eq. 0) array = array * scale_factor
    !
    iret = nf90_get_att(ncid, varid, 'add_offset', add_offset)
    if(iret .eq. 0) array = array + add_offset
    !
    call check_nc_status(nf90_close(ncid), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_v2d_netcdf_R4


  subroutine get_v2d_netcdf_R8(filename, name, array, Xdim, Ydim, rc)
    use netcdf
    implicit none
    character(len=*), intent(in) :: name, filename
    integer, intent(in) :: Xdim, Ydim
    real(REAL64), dimension(Xdim,Ydim), intent(out) :: array
    integer, optional, intent(out) :: rc
    integer :: status
    integer :: ncid, varid
    real    :: scale_factor, add_offset
    integer :: iret

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    call check_nc_status(nf90_inq_varid(ncid, name, varid), _RC)
    call check_nc_status(nf90_get_var(ncid, varid, array), _RC)

    iret = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    if(iret .eq. 0) array = array * scale_factor
    !
    iret = nf90_get_att(ncid, varid, 'add_offset', add_offset)
    if(iret .eq. 0) array = array + add_offset
    !
    call check_nc_status(nf90_close(ncid), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_v2d_netcdf_R8


  subroutine get_v1d_netcdf_R8(filename, name, array, Xdim, group_name, rc)
    use netcdf
    implicit none
    character(len=*), intent(in) :: name, filename
    character(len=*), optional, intent(in) :: group_name
    integer, intent(in) :: Xdim
    real(REAL64), dimension(Xdim), intent(out) :: array
    integer, optional, intent(out) :: rc
    integer :: status
    integer :: ncid, varid, ncid2, ncid_sv

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    ncid_sv = ncid

    if(present(group_name)) then
       if(group_name/='') then
          ncid2= ncid
          call check_nc_status(nf90_inq_ncid(ncid2, group_name, ncid), _RC)
       end if
    end if
    call check_nc_status(nf90_inq_varid(ncid, name, varid), _RC)
    call check_nc_status(nf90_get_var(ncid, varid, array), _RC)

    call check_nc_status(nf90_close(ncid_sv), _RC)
    _RETURN(_SUCCESS)

  end subroutine get_v1d_netcdf_R8


  subroutine get_v1d_netcdf_R8_complete(filename, varname, array, att_name, att_value, group_name, rc)
    use netcdf
    implicit none
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: varname
    real(REAL64), intent(inout) :: array(:)
    character(len=*), optional, intent(in) :: att_name
    real(REAL64), optional, intent(out) :: att_value
    character(len=*), optional, intent(out) :: group_name
    integer, optional, intent(out) :: rc

    integer :: status, iret
    integer :: ncid, ncid_grp, ncid_sv
    integer :: varid
    real(REAL32) :: scale_factor, add_offset

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    ncid_sv = ncid
    if(present(group_name)) then
       if(group_name/='') then
          call check_nc_status(nf90_inq_ncid(ncid, group_name, ncid_grp), _RC)
          ! mod
          ncid = ncid_grp
       end if
    end if
    call check_nc_status(nf90_inq_varid(ncid, varname, varid), _RC)
    call check_nc_status(nf90_get_var(ncid, varid, array), _RC)

    iret = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    if(iret .eq. 0) array = array * scale_factor
    !
    iret = nf90_get_att(ncid, varid, 'add_offset', add_offset)
    if(iret .eq. 0) array = array + add_offset

    if(present(att_name)) then
       call check_nc_status(nf90_get_att(ncid, varid, att_name, att_value), _RC)
    end if

    call check_nc_status(nf90_close(ncid_sv), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_v1d_netcdf_R8_complete


  subroutine get_att_real_netcdf(filename, varname, att_name, att_value, group_name, rc)
    use netcdf
    implicit none
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: att_name
    real(REAL64), intent(out) :: att_value
    character(len=*), optional, intent(out) :: group_name
    integer, optional, intent(out) :: rc
    integer :: status
    integer :: ncid, ncid_grp, ncid_sv
    integer :: varid

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    ncid_sv = ncid
    if(present(group_name)) then
       if(group_name/='') then
          call check_nc_status(nf90_inq_ncid(ncid, group_name, ncid_grp), _RC)
          ! overwrite
          ncid = ncid_grp
       end if
    end if
    call check_nc_status(nf90_inq_varid(ncid, varname, varid), _RC)
    call check_nc_status(nf90_get_att(ncid, varid, att_name, att_value), _RC)
    call check_nc_status(nf90_close(ncid_sv), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_att_real_netcdf

  subroutine get_att_char_netcdf(filename, varname, att_name, att_value, group_name, rc)
    use netcdf
    implicit none
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: att_name
    character(len=*), intent(out) :: att_value
    character(len=*), optional, intent(out) :: group_name
    integer, optional, intent(out) :: rc
    integer :: status
    integer :: ncid, ncid_grp, ncid_sv
    integer :: varid

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    ncid_sv = ncid
    if(present(group_name)) then
       if(group_name/='') then
          call check_nc_status(nf90_inq_ncid(ncid, group_name, ncid_grp), _RC)
          ! overwrite
          ncid = ncid_grp
       end if
    end if
    call check_nc_status(nf90_inq_varid(ncid, varname, varid), _RC)
    call check_nc_status(nf90_get_att(ncid, varid, att_name, att_value), _RC)
    call check_nc_status(nf90_close(ncid_sv), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_att_char_netcdf


  subroutine check_nc_status(status, rc)
    use netcdf
    implicit none
    integer, intent(in) :: status
    integer, intent(out), optional :: rc

    _ASSERT(status == nf90_noerr, 'netCDF error: '//trim(nf90_strerror(status)))
    _RETURN(_SUCCESS)

  end subroutine check_nc_status


  subroutine time_nc_int_2_esmf(time, tunit, n, rc)
    use ESMF
    implicit none

    type(ESMF_TIME), intent(out) :: time
    integer, intent(in) :: n
    character(len=*), intent(in) :: tunit
    integer, intent(out), optional :: rc
    integer :: status

    type(ESMF_Time) :: time0
    type(ESMF_TimeInterval) :: dt
    integer :: iyy,imm,idd,ih,im,is

    call parse_timeunit(tunit, n, time0, dt, _RC)
    time = time0 + dt

    ! check
    ! -----
    call ESMF_timeGet(time, yy=iyy, mm=imm, dd=idd, h=ih, m=im, s=is, _RC)
    write(6, *) 'obs_start: iyy,imm,idd,ih,im,is', iyy,imm,idd,ih,im,is

    _RETURN(_SUCCESS)

  end subroutine time_nc_int_2_esmf


  subroutine time_esmf_2_nc_int(time, tunit, n, rc)
    use ESMF
    implicit none

    type(ESMF_TIME), intent(in) :: time
    integer(ESMF_KIND_I8), intent(out) :: n
    character(len=*), intent(in) :: tunit
    integer, intent(out), optional :: rc
    integer :: status

    type(ESMF_Time) :: time0
    type(ESMF_TimeInterval) :: dt

    n=0
    call parse_timeunit(tunit, n, time0, dt, _RC)
    dt = time - time0

    ! assume unit is second
    !
    call ESMF_TimeIntervalGet(dt, s_i8=n, _RC)

    _RETURN(_SUCCESS)

  end subroutine time_esmf_2_nc_int


  !
  ! n sec after tunit
  ! t0 = since [ xxxx-xx-xx ]
  ! dt = n sec
  subroutine parse_timeunit_i4(tunit, n, t0, dt, rc)
    use ESMF
    implicit none

    character(len=*), intent(in) :: tunit
    integer, intent(in) :: n
    type(ESMF_Time), intent(out) :: t0
    type(ESMF_TimeInterval), intent(out) :: dt
    integer, optional, intent(out) :: rc
    integer :: status
    integer(ESMF_KIND_I8) :: n8

    n8 = n
    call parse_timeunit(tunit, n8, t0, dt, _RC)
   _RETURN(_SUCCESS)

  end subroutine parse_timeunit_i4


  subroutine parse_timeunit_i8(tunit, n, t0, dt, rc)
    use ESMF
    implicit none

    character(len=*), intent(in) :: tunit
    integer(ESMF_KIND_I8), intent(in) :: n
    type(ESMF_Time), intent(out) :: t0
    type(ESMF_TimeInterval), intent(out) :: dt
    integer, optional, intent(out) :: rc
    integer :: status

    integer :: i
    character(len=ESMF_MAXSTR) :: s1, s2, s_time, s_unit
    character(len=1) :: c1
    integer :: y,m,d,hour,min,sec
    integer(ESMF_KIND_I8) :: isec

    i=index(trim(tunit), 'since')
    s_time=trim(tunit(i+5:))
    s_unit=trim(tunit(1:i-1))
    read(s_time,*) s1, s2
    read(s1, '(i4,a1,i2,a1,i2)') y, c1, m, c1, d
    read(s2, '(i2,a1,i2,a1,i2)') hour, c1, min, c1, sec

!    write(6,*) 'y, c1, m, c1, d',  y, c1, m, c1, d
!    write(6,*) 'hour, c1, min, c1, sec', hour, c1, min, c1, sec

    if (trim(s_unit) == 'seconds') then
       isec=n
    elseif (trim(s_unit) == 'minutes') then
       isec=n * 60
    elseif (trim(s_unit) == 'hours') then
       isec=n * 3600
    else
       _FAIL ('time_unit not implemented')
    end if

    call ESMF_timeSet(t0, yy=y,mm=m,dd=d,h=hour,m=min,s=sec, _RC)
    call ESMF_timeintervalSet(dt, d=0, h=0, m=0, s_i8=isec, _RC)
    _RETURN(_SUCCESS)

  end subroutine parse_timeunit_i8


  subroutine diff_two_timeunits (tunit1, tunit2, x, dt_esmf, rc)
    character(len=*), intent(in) :: tunit1
    character(len=*), intent(in) :: tunit2
    real(ESMF_KIND_R8), intent(out) :: x
    type(ESMF_TimeInterval), optional, intent(out) :: dt_esmf
    integer, intent(out), optional     :: rc

    type(ESMF_Time) :: t1_base
    type(ESMF_TimeInterval) :: dt1
    type(ESMF_Time) :: t2_base
    type(ESMF_TimeInterval) :: dt2
    type(ESMF_TimeInterval) :: deltaT_base
    integer(ESMF_KIND_I8) :: n1
    integer(ESMF_KIND_I8) :: n2
    character(len=20) :: s_unit
    integer :: i, status, sec

    n1=0; n2=0
    call parse_timeunit (tunit1, n1, t1_base, dt1, _RC)
    call parse_timeunit (tunit2, n2, t2_base, dt2, _RC)
    deltaT_base = t2_base - t1_base
    if (present(dt_esmf)) dt_esmf = deltaT_base

    i=index(trim(tunit1), 'since')
    s_unit=trim(tunit1(1:i-1))

    call ESMF_TimeIntervalGet(deltaT_base, s=sec, _RC)
    if (trim(s_unit) == 'seconds') then
       x = sec
    elseif (trim(s_unit) == 'minutes') then
       x = sec / 60.d0
    elseif (trim(s_unit) == 'hours') then
       x = sec /3600.d0
    else
       _FAIL ('time_unit not implemented')
    end if

    !!write(6,*) 'tunit1=', tunit1
    !!write(6,*) 'tunit2=', tunit2
    !!write(6,*) 'del sec', sec
    !!write(6,*) 'del x',  x

    _RETURN(ESMF_SUCCESS)
  end subroutine diff_two_timeunits


  subroutine ESMF_time_to_two_integer(time, itime, rc)
    type(ESMF_Time), intent(in) ::   time
    integer, intent(out) :: itime(2)
    integer, intent(out), optional :: rc
    integer :: i1, i2
    integer :: yy, mm, dd, h, m, s
    integer :: status

    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)

    i2=h*10000 + m*100 + s
    i1=yy*10000 + mm*100 + dd
    itime(1)=i1
    itime(2)=i2

    _RETURN(_SUCCESS)

  end subroutine ESMF_time_to_two_integer


  subroutine two_integer_to_ESMF_time(time, itime, rc)
    type(ESMF_Time), intent(out) ::   time
    integer, intent(in) :: itime(2)
    integer, intent(out), optional :: rc
    integer :: i1, i2
    integer :: yy, mm, dd, h, m, s
    integer :: status

    i1= itime(1)
    yy= i1/10000
    mm= mod(i1, 10000)/100
    dd= mod(i1, 100)

    i2= itime(2)
    h= i2/10000
    m= mod(i2, 10000)/100
    s= mod(i2, 100)

    call ESMF_TimeSet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)

    _RETURN(_SUCCESS)

  end subroutine two_integer_to_ESMF_time


  integer function hms_2_s(hms)
    integer, intent(in) :: hms
    integer :: h, m, s

    h = hms/10000
    m = mod(hms, 10000)/100
    s = mod(hms, 100)

    hms_2_s = h*3600 + m*60 + s

  end function hms_2_s


  subroutine bisect_find_LB_R8_I8(xa, x, n, n_LB, n_UB, rc)
    implicit none
    real(ESMF_KIND_R8), intent(in) :: xa(:)   ! 1D array
    real(ESMF_KIND_R8), intent(in) :: x       ! pt
    integer(ESMF_KIND_I8), intent(out) :: n   !  out: bisect index
    integer(ESMF_KIND_I8), intent(in), optional :: n_LB  !  opt in : LB
    integer(ESMF_KIND_I8), intent(in), optional :: n_UB  !  opt in : UB
    integer, intent(out), optional :: rc
    integer :: status

    integer(ESMF_KIND_I8) :: k, klo, khi, dk, LB, UB
    integer :: i, nmax

    LB=1; UB=size(xa,1)
    if(present(n_LB)) LB=max(LB, n_LB)
    if(present(n_UB)) UB=min(UB, n_UB)
    klo=LB; khi=UB; dk=1

    if ( xa(LB ) > xa(UB) )  then
       klo= UB
       khi= LB
       dk= -1
    endif

    !    ----|---------------------------|--------->
    !  Y:   klo                         khi
    !     x         x                       x
    !
    !        Y(n)  <  x  <=  Y(n+1)

    status=-1
    if ( x <= xa(klo) ) then
       n=klo-1
       return
    elseif ( x > xa(khi) ) then
       n=khi
       return
    endif

    nmax = log(abs(real(khi-klo))) / log(2.0) + 2  ! LOG2(M)
    do i = 1, nmax
       k=(klo+khi)/2
       if ( x <= xa(k) ) then
          khi = k
       else
          klo = k
       endif
       if( abs(klo-khi) <= 1 ) then
          n=klo
          status=0
          exit
       endif
    enddo

    _RETURN(_SUCCESS)

  end subroutine bisect_find_LB_R8_I8


  subroutine convert_twostring_2_esmfinterval(symd, shms, interval, rc)
    character(len=*) :: symd
    character(len=*) :: shms
    type(ESMF_TimeInterval), intent(out) :: interval
    integer, optional, intent(out) :: rc
    character(len=20) :: s1, s2
    integer :: y, m, d, hh, mm, ss
    integer :: status

    s1=trim(symd)
    read(s1, '(3i2)') y, m, d
    s2=trim(shms)
    read(s2, '(3i2)') hh, mm, ss

    call ESMF_TimeIntervalSet(interval, yy=y, mm=m, d=d, h=hh, m=mm, s=ss, _RC)

    _RETURN(_SUCCESS)

  end subroutine convert_twostring_2_esmfinterval

end module Plain_NetCDF_Time


module MAPL_scan_pattern_in_file

!  procedure :: matchbgn
!  procedure :: matches
!  procedure :: scan_begin
!  procedure :: scan_contain
!  generic :: scan_count_matchbgn
!  generic :: go_last_pattern

contains
  subroutine scan_begin (iunps, substring, rew)
    implicit none
    ! unit of input
    integer, intent(in) :: iunps
    ! Label to be matched
    character (len=*), intent(in) :: substring
    logical, intent(in) :: rew
    ! String read from file
    character (len=100) :: line
    ! Flag if .true. rewind the file
    !logical, external :: matchbgn
!    logical :: matchbgn
    integer :: ios
    !
    ios = 0
    if (rew) rewind (iunps)
    do while (ios==0)
       read (iunps, '(a100)', iostat = ios) line
       if (matchbgn (trim(adjustl(line)), trim(substring)) ) return
    enddo
    return
  end subroutine scan_begin


  subroutine scan_contain (iunps, stop_string, rew)
    !---------------------------------------------------------------------
    !
    implicit none
    integer, intent(in) :: iunps
    character (len=*), intent(in) :: stop_string
    logical, intent(in) :: rew            ! if rewind
    character (len=100) :: line
!!    logical :: matches          ! function name
    integer :: ios
    !
    ios = 0
    if (rew) rewind (iunps)
    do while (ios==0)
       read (iunps, '(a100)', iostat = ios) line
       if (matches (trim(line), trim(stop_string)) ) return
    enddo
    return
  end subroutine scan_contain



  subroutine scan_count_match_bgn (iunps, string, count, rew)
    !---------------------------------------------------------------------
    !
    implicit none
    integer, intent(in) :: iunps
    character (len=*), intent(in) :: string
    integer, intent(out) :: count
    logical, intent(in) :: rew            ! if rewind
    character (len=100) :: line
!!    logical :: matches          ! function name
    integer :: ios
    !
    ios = 0
    count = 0
    if (rew) rewind (iunps)
    do while (ios==0)
       read (iunps, '(a100)', iostat = ios) line
       if (matchbgn (adjustl(line), string) ) then
          count = count + 1
       endif
    enddo
    return
  end subroutine scan_count_match_bgn


  subroutine go_last_patn (iunps, substring, outline, rew)
    !---------------------------------------------------------------------
    !
    implicit none
    integer, intent(in) :: iunps
    logical, intent(in) :: rew
    character (len=*), intent(in) :: substring
    character (len=150), intent(out) :: outline   ! fixed
    character (len=150) :: line
    integer :: ios, nr, mx

    if (rew) rewind (iunps)
    ios=0
    nr=0
    do while (ios==0)
       read (iunps, '(a150)', iostat = ios, err = 300) line
       if (index(line, substring).ge.1 ) then
          nr=nr+1
          !        write (6,*) 'nr', nr
       endif
    enddo

    rewind (iunps)
    ios=0
    mx=0
    do while (ios==0)
       read (iunps, '(a150)', iostat = ios, err = 300) line
       if (index(line, substring).ge.1 ) then
          mx=mx+1
          if (mx.eq.nr) then
             outline=line
             return
          endif
       endif
    enddo
300 continue
  end subroutine go_last_patn


  function matchbgn ( string, substring )
    ! only begin with
    ! string:  main-str
    ! substring:  sub-str
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: string, substring
    LOGICAL                       :: matchbgn
    if (index(string, substring).eq.1) then
       matchbgn = .TRUE.
    else
       matchbgn = .FALSE.
    endif
    return
  end function matchbgn


  !-----------------------------------------------------------------------
  function matches( string, substring )
    !-----------------------------------------------------------------------
    !
    ! ... .TRUE. if string is contained in substring, .FALSE. otherwise
    !
    IMPLICIT NONE
    !
    CHARACTER (LEN=*), INTENT(IN) :: string, substring
    LOGICAL                       :: matches
    INTEGER                       :: l

    l=index (string, substring)
    if (l.ge.1) then
       matches = .TRUE.
    else
       matches = .FALSE.
    endif
    RETURN
  end function matches


  subroutine split_string_by_space (string_in, length_mx, &
       mxseg, nseg, str_piece, jstatus)
    integer,           intent (in) :: length_mx
    character (len=length_mx), intent (in) :: string_in
    integer,           intent (in) :: mxseg
    integer,           intent (out):: nseg
    character (len=length_mx), intent (out):: str_piece(mxseg)
    integer,           intent (out):: jstatus
    character (len=length_mx) :: string
    character (len=1) :: mark
    integer :: ios
    integer :: wc
    !
    !  "xxxx  yy zz   uu   vv"
    !
    ! split by space ''
    mark=' '
    wc=0
    ios=0
    string = trim( adjustl(string_in) )
    str_piece(:)=''
    i = index (string, mark)
    if (i==0) then
       nseg=1
       str_piece(1)=string
       return
    end if
    do while (ios==0)
       i = index (string, mark)
       !!print*, 'index=', i
       if (i > 1) then
          wc = wc + 1
          str_piece(wc)=trim(adjustl(string(1:i)))
          !!write(6,*) 'str_piece(wc)=', trim(str_piece(wc))
          string = trim(adjustl(string(i:)))
       else
          ios=1
       end if
       if (LEN_TRIM(adjustl(string)) == 0) ios=1
    end do
    nseg=wc
    return
  end subroutine split_string_by_space


  subroutine split_string_by_seperator (string_in, length_mx, seperator_in, &
       mxseg, nseg, str_piece, jstatus)
    character (len=length_mx), intent (in) :: string_in
    integer,           intent (in) :: length_mx
    character (len=length_mx), intent (in) :: seperator_in
    integer,           intent (in) :: mxseg
    integer,           intent (out):: nseg
    character (len=length_mx), intent (out):: str_piece(mxseg)
    integer,           intent (out):: jstatus
    character (len=length_mx) :: string_sc, string_oper, string_aux
    character (len=1) :: mark, CH
    integer :: ios
    integer :: wc
    integer :: len1, len2
    !
    !  "xxxx;  yy; zz;   uu,   vv,"
    !  seperator = ";,"
    !

    !__ s1. replace seperator by space
    !
    string_sc = trim( adjustl(string_in) )
    string_oper = trim( adjustl(seperator_in) )
    len1 = len_trim(string_sc)
    len2 = len_trim(string_oper)
    string_aux=string_sc
    do i = 1, len1
       CH = string_sc(i:i)
       do j = 1, len2
          mark = string_oper(j:j)
          if (CH==mark) then
             string_aux(i:i)=' '
          end if
       end do
    end do

    !__ s2. split by space
    call split_string_by_space (string_aux, length_mx, &
       mxseg, nseg, str_piece, jstatus)

    return
  end subroutine split_string_by_seperator


  
  
  subroutine scan_write_between_line1_line2_flush_Left (ur, uw, L1, L2)
    character (len=*), intent(in) :: L1, L2
    integer, intent(in) :: ur, uw

    integer :: i, j, k
    character (len=150) :: line

    rewind(ur)
    ios=0
    j=0
    k=0
    do while (ios==0)
       read (ur, '(a150)', iostat = ios, err = 300) line
       i=index( adjustl(line), trim(L1) )
       if ( i==1 ) then
          j=1
       end if
       if (j==1) then
          write(uw, '(a)')  trim(line)
          k=index( adjustl(line), trim(L2) )
          if (k==1) then
             j=0
          end if
       end if
    end do
300 continue
    write(uw,*)

  end subroutine scan_write_between_line1_line2_flush_Left


  subroutine scan_write_begin_with_line1_flush_Left (ur, uw, L1)
    character (len=*), intent(in) :: L1
    integer, intent(in) :: ur, uw

    integer :: i  
    character (len=300) :: line

    rewind(ur)
    ios=0
    do while (ios==0)
       read (ur, '(a300)', iostat = ios, err = 300) line
       i=index( adjustl(line), trim(L1) )
       if ( i==1 ) then
          write(uw, '(a)')  trim(line)
       end if
    end do
300 continue
    write(uw,*)

  end subroutine scan_write_begin_with_line1_flush_Left


end module MAPL_scan_pattern_in_file

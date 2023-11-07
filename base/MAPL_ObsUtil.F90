#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_ObsUtilMod
  use ESMF
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  implicit none
  integer, parameter :: mx_ngeoval = 60
  private

  public :: obs_unit
  type :: obs_unit
     integer :: nobs_epoch
     integer :: ngeoval
     logical :: export_all_geoval
     type(FileMetadata), allocatable :: metadata
     type(NetCDF4_FileFormatter), allocatable :: file_handle
     character(len=ESMF_MAXSTR) :: name
     character(len=ESMF_MAXSTR) :: obsFile_output
     character(len=ESMF_MAXSTR) :: input_template
     character(len=ESMF_MAXSTR) :: geoval_name(mx_ngeoval)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL64), allocatable :: times_R8(:)
     real(kind=REAL32), allocatable :: p2d(:)
     real(kind=REAL32), allocatable :: p3d(:,:)
  end type obs_unit

  interface sort_multi_arrays_by_time
     module procedure sort_three_arrays_by_time
     module procedure sort_four_arrays_by_time
  end interface sort_multi_arrays_by_time

contains

  subroutine get_obsfile_Tbracket_from_epoch(currTime, &
       obsfile_start_time, obsfile_end_time, obsfile_interval, &
       epoch_frequency, obsfile_Ts_index, rc)
    implicit none
    type(ESMF_Time), intent(in) :: currTime
    type(ESMF_Time), intent(in) :: obsfile_start_time, obsfile_end_time
    type(ESMF_TimeInterval), intent(in) :: obsfile_interval, epoch_frequency
    integer, intent(out) :: obsfile_Ts_index
    integer, intent(out) :: obsfile_Te_index
    integer, optional, intent(out) :: rc

    type(ESMF_Time) :: T1, Tn
    type(ESMF_Time) :: cT1
    type(ESMF_Time) :: Ts, Te
    type(ESMF_TimeInterval) :: dT1, dT2, dTs, dTe
    real(ESMF_KIND_R8) :: dT0_s, dT1_s, dT2_s
    real(ESMF_KIND_R8) :: s1, s2
    integer :: n1, n2
    integer :: status

    T1 = obsfile_start_time
    Tn = obsfile_end_time

    cT1 = currTime
    dT1 = currTime - T1
    dT2 = currTime + epoch_frequency - T1

    call ESMF_TimeIntervalGet(obsfile_interval, s_r8=dT0_s, rc=status)
    call ESMF_TimeIntervalGet(dT1, s_r8=dT1_s, rc=status)
    call ESMF_TimeIntervalGet(dT2, s_r8=dT2_s, rc=status)
    n1 = floor (dT1_s / dT0_s)
    n2 = floor (dT2_s / dT0_s)
    s1 = n1 * dT0_s
    s2 = n2 * dT0_s
    call ESMF_TimeIntervalSet(dTs, s_r8=s1, rc=status)
    call ESMF_TimeIntervalSet(dTe, s_r8=s2, rc=status)
    Ts = T1 + dTs
    Te = T1 + dTe

    obsfile_Ts_index = n1
    if ( dT2_s - n2*dT0_s < 1 ) then
       obsfile_Te_index = n2 - 1
    else
       obsfile_Te_index = n2
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine get_obsfile_Tbracket_from_epoch

  function get_filename_from_template (time, file_template, rc) result(filename)
    type(ESMF_Time), intent(in) :: time
    character(len=*), intent(in) :: file_template
    character(len=ESMF_MAXSTR) :: filename
    integer, optional, intent(out) :: rc


    integer :: itime(2)
    integer :: nymd, nhms
    integer :: status

    stop 'DO not use get_filename_from_template'
    call ESMF_time_to_two_integer(time, itime, _RC)
    print*, 'two integer time, itime(:)', itime(1:2)
    nymd = itime(1)
    nhms = itime(2)
    call fill_grads_template ( filename, file_template, &
         experiment_id='', nymd=nymd, nhms=nhms, _RC )
    print*, 'ck: this%obsFile_T=', trim(filename)
    _RETURN(ESMF_SUCCESS)

  end function get_filename_from_template


  function get_filename_from_template_use_index (obsfile_start_time, obsfile_interval, &
       f_index, file_template, rc) result(filename)
    character(len=*), intent(in) :: file_template
    character(len=ESMF_MAXSTR) :: filename
    integer, intent(in) :: f_index
    integer, optional, intent(out) :: rc

    integer :: itime(2)
    integer :: nymd, nhms
    integer :: status
    real(ESMF_KIND_R8) :: dT0_s
    real(ESMF_KIND_R8) :: s
    type(ESMF_TimeInterval) :: dT
    type(ESMF_Time) :: time

    call ESMF_TimeIntervalGet(this%obsfile_interval, s_r8=dT0_s, rc=status)
    s = dT0_s * f_index
    call ESMF_TimeIntervalSet(dT, s_r8=s, rc=status)
    time = this%obsfile_start_time + dT

    call ESMF_time_to_two_integer(time, itime, _RC)
    nymd = itime(1)
    nhms = itime(2)
    call fill_grads_template ( filename, file_template, &
         experiment_id='', nymd=nymd, nhms=nhms, _RC )

    _RETURN(ESMF_SUCCESS)

  end function get_filename_from_template_use_index



  subroutine time_real_to_ESMF (times_R8_1d, times_esmf_1d, datetime_units, rc)
    real(kind=REAL64), intent(in) :: times_R8_1d(:)
    type(ESMF_Time), intent(in) :: times_esmf_1d(:)
    character(len=*), intent(in) :: datetime_units
    integer, optional, intent(out) :: rc

    type(ESMF_TimeInterval) :: interval
    type(ESMF_Time) :: time0
    type(ESMF_Time) :: time1
    character(len=:), allocatable :: tunit

    integer :: i, len
    integer :: int_time
    integer :: status

    len = size (this%times_R8_1d)
    do i=1, len
       int_time = times_R8_1d(i)
       call convert_NetCDF_DateTime_to_ESMF(int_time, datetime_units, interval, &
            time0, &
            time=time1, &
            time_unit=tunit, &
            _RC)
       times_esmf_1d(i) = time1
    enddo

    _RETURN(_SUCCESS)
  end subroutine time_real_to_ESMF


  subroutine reset_times_to_current_day(current_time, times_1d, rc)
    type(ESMF_Time), intent(in) :: current_time
    type(ESMF_Time), intent(inout) :: times_1d(:)
    integer, optional, intent(out) :: rc
    integer :: i,status,h,m,yp,mp,dp,s,ms,us,ns
    integer :: year,month,day

    call ESMF_TimeGet(current_time,yy=year,mm=month,dd=day,_RC)
    do i=1,size(times_1d)
       call ESMF_TimeGet(times_1d(i),yy=yp,mm=mp,dd=dp,h=h,m=m,s=s,ms=ms,us=us,ns=ns,_RC)
       call ESMF_TimeSet(times_1d(i),yy=year,mm=month,dd=day,h=h,m=m,s=s,ms=ms,us=us,ns=ns,_RC)
    enddo
    _RETURN(_SUCCESS)
  end subroutine reset_times_to_current_day


  subroutine sort_three_arrays_by_time(U,V,T,rc)
    real(ESMF_KIND_R8), intent(inout) :: U(:), V(:), T(:)
    integer, optional, intent(out) :: rc

    integer :: i, len
    integer, allocatable :: IA(:)
    integer(ESMF_KIND_I8), allocatable :: IX(:)
    real(ESMF_KIND_R8), allocatable :: X(:)

    _ASSERT (size(U)==size(V), 'U,V different dimension')
    _ASSERT (size(U)==size(T), 'U,T different dimension')
    len = size (T)

    allocate (IA(len), IX(len), X(len))
    do i=1, len
       IX(i)=T(i)
       IA(i)=i
    enddo
    call MAPL_Sort(IX,IA)

    X = U
    do i=1, len
       U(i) = X(IA(i))
    enddo
    X = V
    do i=1, len
       V(i) = X(IA(i))
    enddo
    X = T
    do i=1, len
       T(i) = X(IA(i))
    enddo
    _RETURN(_SUCCESS)
  end subroutine sort_three_arrays_by_time

  subroutine sort_four_arrays_by_time(U,V,T,ID,rc)
    real(ESMF_KIND_R8) :: U(:), V(:), T(:)
    integer :: ID(:)
    integer, optional, intent(out) :: rc

    integer :: i, len
    integer, allocatable :: IA(:)
    integer(ESMF_KIND_I8), allocatable :: IX(:)
    real(ESMF_KIND_R8), allocatable :: X(:)
    integer, allocatable :: NX(:)

    _ASSERT (size(U)==size(V), 'U,V different dimension')
    _ASSERT (size(U)==size(T), 'U,T different dimension')
    len = size (T)

    allocate (IA(len), IX(len), X(len), NX(len))
    do i=1, len
       IX(i)=T(i)
       IA(i)=i
    enddo
    call MAPL_Sort(IX,IA)

    X = U
    do i=1, len
       U(i) = X(IA(i))
    enddo
    X = V
    do i=1, len
       V(i) = X(IA(i))
    enddo
    X = T
    do i=1, len
       T(i) = X(IA(i))
    enddo
    NX = ID
    do i=1, len
       ID(i) = NX(IA(i))
    enddo
    _RETURN(_SUCCESS)
  end subroutine sort_four_arrays_by_time

end module MAPL_ObsUtilMod

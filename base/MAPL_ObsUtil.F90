#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_ObsUtilMod
  use ESMF
  use Plain_netCDF_Time
  use netCDF
  use MAPL_CommsMod, only : MAPL_AM_I_ROOT
  use pFIO_FileMetadataMod, only : FileMetadata
  use pFIO_NetCDF4_FileFormatterMod, only : NetCDF4_FileFormatter
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  implicit none
  integer, parameter :: mx_ngeoval = 60
!!  private

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

  type obs_platform
     character (len=ESMF_MAXSTR) :: name=''
     character (len=ESMF_MAXSTR) :: index_name_x=''
     character (len=ESMF_MAXSTR) :: var_name_lon=''
     character (len=ESMF_MAXSTR) :: var_name_lat=''
     character (len=ESMF_MAXSTR) :: var_name_time=''
     character (len=ESMF_MAXSTR) :: file_name_template=''
     integer :: ngeoval=0
     integer :: nentry_name=0
     character (len=ESMF_MAXSTR), allocatable :: field_name(:,:)
     !character (len=ESMF_MAXSTR), allocatable :: field_name(:)
  end type obs_platform

  interface sort_multi_arrays_by_time
     module procedure sort_three_arrays_by_time
     module procedure sort_four_arrays_by_time
  end interface sort_multi_arrays_by_time

contains

  subroutine get_obsfile_Tbracket_from_epoch(currTime, &
       obsfile_start_time, obsfile_end_time, obsfile_interval, &
       epoch_frequency, obsfile_Ts_index, obsfile_Te_index, rc)
    implicit none
    type(ESMF_Time), intent(in) :: currTime
    type(ESMF_Time), intent(in) :: obsfile_start_time, obsfile_end_time
    type(ESMF_TimeInterval), intent(in) :: obsfile_interval, epoch_frequency
    integer, intent(out) :: obsfile_Ts_index
    integer, intent(out) :: obsfile_Te_index
    integer, optional, intent(out) :: rc

    type(ESMF_Time) :: T1
    type(ESMF_Time) :: cT1
    type(ESMF_Time) :: Ts, Te
    type(ESMF_TimeInterval) :: dT1, dT2, dTs, dTe
    real(ESMF_KIND_R8) :: dT0_s, dT1_s, dT2_s
    real(ESMF_KIND_R8) :: s1, s2
    integer :: n1, n2
    integer :: status

    !
    !  o---------o ------------- o -------------o
    !             obsfile_interval
    !               x---------------------x--
    !                       Epoch
    !

    T1 = obsfile_start_time

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
    obsfile_Te_index = n2

    _RETURN(ESMF_SUCCESS)

  end subroutine get_obsfile_Tbracket_from_epoch


  function get_filename_from_template (time, file_template, rc) result(filename)
    use Plain_netCDF_Time, only : ESMF_time_to_two_integer
    use  MAPL_StringTemplate, only : fill_grads_template
    type(ESMF_Time), intent(in) :: time
    character(len=*), intent(in) :: file_template
    character(len=ESMF_MAXSTR) :: filename
    integer, optional, intent(out) :: rc

    integer :: itime(2)
    integer :: nymd, nhms
    integer :: status

    _FAIL ('DO not use get_filename_from_template')
    call ESMF_time_to_two_integer(time, itime, _RC)
    print*, 'two integer time, itime(:)', itime(1:2)
    nymd = itime(1)
    nhms = itime(2)
    call fill_grads_template ( filename, file_template, &
         experiment_id='', nymd=nymd, nhms=nhms, _RC )
    print*, 'ck: obsFile_T=', trim(filename)
    _RETURN(ESMF_SUCCESS)

  end function get_filename_from_template


  subroutine time_real_to_ESMF (times_R8_1d, times_esmf_1d, datetime_units, rc)
    use  MAPL_NetCDF, only : convert_NetCDF_DateTime_to_ESMF

    real(kind=REAL64), intent(in) :: times_R8_1d(:)
    type(ESMF_Time), intent(inout) :: times_esmf_1d(:)
    character(len=*), intent(in) :: datetime_units
    integer, optional, intent(out) :: rc

    type(ESMF_TimeInterval) :: interval
    type(ESMF_Time) :: time0
    type(ESMF_Time) :: time1
    character(len=:), allocatable :: tunit

    integer :: i, len
    integer :: int_time
    integer :: status

    len = size (times_R8_1d)
    do i=1, len
       int_time = times_R8_1d(i)
       call convert_NetCDF_DateTime_to_ESMF(int_time, datetime_units, interval, &
            time0, time=time1, time_unit=tunit, _RC)
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




  !  --//-------------------------------------//->
  !   files
  !      o   o   o   o   o   o   o     o   o   o  T: filename
  !  <--- off set
  !  o   o   o   o   o    o   o     o   o   o     T: file content start
  !          |                    |
  !         curr                curr+Epoch
  !

  subroutine Find_M_files_for_currTime (currTime, &
       obsfile_start_time, obsfile_end_time, obsfile_interval, &
       epoch_frequency, file_template, M, filenames, &
       T_offset_in_file_content, rc)
    implicit none
    type(ESMF_Time), intent(in) :: currTime
    type(ESMF_Time), intent(in) :: obsfile_start_time, obsfile_end_time
    type(ESMF_TimeInterval), intent(in) :: obsfile_interval, epoch_frequency
    character(len=*), intent(in) :: file_template
    integer, intent(out) :: M
    character(len=ESMF_MAXSTR), intent(inout) :: filenames(:)
    type(ESMF_TimeInterval), intent(in), optional :: T_offset_in_file_content
    integer, optional, intent(out) :: rc

    type(ESMF_Time) :: T1, Tn
    type(ESMF_Time) :: cT1
    type(ESMF_Time) :: Ts, Te
    type(ESMF_TimeInterval) :: dT1, dT2, dTs, dTe
    type(ESMF_TimeInterval) :: Toff
    real(ESMF_KIND_R8) :: dT0_s, dT1_s, dT2_s
    real(ESMF_KIND_R8) :: s1, s2
    character(len=ESMF_MAXSTR) :: test_file

    integer :: obsfile_Ts_index, obsfile_Te_index
    integer :: n1, n2
    integer :: i, j
    integer :: status
    logical :: exist

    !__ s1.  Arithmetic index list based on s,e,interval
    !
    if (present(T_offset_in_file_content)) then
       Toff = T_offset_in_file_content
    else
       call ESMF_TimeIntervalSet(Toff, h=0, m=0, s=60, rc=status)
    endif

    !    T1 = obsfile_start_time + Toff
    !    Tn = obsfile_end_time + Toff

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

!    print*, 'ck dT0_s, dT1_s, dT2_s', dT0_s, dT1_s, dT2_s
!    print*, '1st n1, n2', n1, n2

    obsfile_Ts_index = n1
    if ( dT2_s - n2*dT0_s < 1 ) then
       obsfile_Te_index = n2 - 1
    else
       obsfile_Te_index = n2
    end if

    ! put back
    n1 = obsfile_Ts_index
    n2 = obsfile_Te_index

!    print*, __LINE__, __FILE__
!    print*, '2nd n1, n2', n1, n2

    !__ s2.  further test file existence
    !
    j=0
    do i= n1, n2
       test_file = get_filename_from_template_use_index &
            (obsfile_start_time, obsfile_interval, &
            i, file_template, exist, rc=rc)
       if (exist) then
          j=j+1
          filenames(j) = test_file
       end if
    end do
    M=j

    _ASSERT ( M < size(filenames) , 'code crash, number of files exceeds upper bound')
    _ASSERT (M/=0, 'M is zero, no files found for currTime')

    _RETURN(_SUCCESS)

  end subroutine Find_M_files_for_currTime


  subroutine read_M_files_4_swath ( filenames, Xdim, Ydim, &
       index_name_lon, index_name_lat,&
       var_name_lon, var_name_lat, var_name_time, &
       lon, lat, time, Tfilter, rc )
    use pFlogger, only: logging, Logger
    character(len=ESMF_MAXSTR), intent(in) :: filenames(:)
    integer,  intent(out) :: Xdim
    integer,  intent(out) :: Ydim
    character(len=ESMF_MAXSTR), intent(in) :: index_name_lon
    character(len=ESMF_MAXSTR), intent(in) :: index_name_lat
    character(len=ESMF_MAXSTR), optional, intent(in) :: var_name_lon
    character(len=ESMF_MAXSTR), optional, intent(in) :: var_name_lat
    character(len=ESMF_MAXSTR), optional, intent(in) :: var_name_time
    real(ESMF_KIND_R8), allocatable, optional, intent(inout) :: lon(:,:)
    real(ESMF_KIND_R8), allocatable, optional, intent(inout) :: lat(:,:)
    real(ESMF_KIND_R8), allocatable, optional, intent(inout) :: time(:,:)    
    logical, optional, intent(in)  ::  Tfilter
    integer, optional, intent(out) :: rc

    integer :: M
    integer :: i, j, jx, j2, status
    integer :: nlon, nlat
    integer :: ncid, ncid2
    character(len=ESMF_MAXSTR) :: grp1, grp2
    integer :: varid
    logical :: found_group

    character(len=ESMF_MAXSTR) :: filename
    integer, allocatable :: nlons(:), nlats(:)
    real(ESMF_KIND_R8), allocatable :: time_loc_R8(:,:)
    real(ESMF_KIND_R8), allocatable :: lon_loc(:,:)
    real(ESMF_KIND_R8), allocatable :: lat_loc(:,:)
    class(Logger), pointer :: lgr

    !__ s1. get Xdim Ydim
    M = size(filenames)
    _ASSERT(M/=0, 'M is zero, no files found')
    lgr => logging%get_logger('MAPL.Sampler')

    allocate(nlons(M), nlats(M))
    jx=0
    do i = 1, M
       filename = filenames(i)
       CALL get_ncfile_dimension(filename, nlon=nlon, nlat=nlat, &
            key_lon=index_name_lon, key_lat=index_name_lat, _RC)
       nlons(i)=nlon
       nlats(i)=nlat
       jx=jx+nlat

       call lgr%debug('Input filename: %a', trim(filename))
       call lgr%debug('Input file    : nlon, nlat= %i6  %i6', nlon, nlat)
    end do
    !
    ! __ output results wo filter
    !
    Xdim=nlon
    Ydim=jx
    j2=jx

    !__ s2. get fields

    if ( present(Tfilter) .AND. Tfilter ) then
       if ( .not. (present(time) .AND. present(lon) .AND. present(lat)) ) then
          _FAIL('when Tfilter present, time/lon/lat must also present')
       end if

       !
       ! -- determine jx
       !
       jx=0
       do i = 1, M
          filename = filenames(i)
          nlon = nlons(i)
          nlat = nlats(i)
          allocate (time_loc_R8(nlon, nlat))
          call get_var_from_name_w_group (var_name_time, time_loc_R8, filename, _RC)
!!          write(6,*) 'af ith, filename', i, trim(filename)

          do j=1, nlat
             !
             ! -- filter, e.g., eliminate -9999
             !
             if ( time_loc_R8(1, j) > 0.0 ) then
                jx = jx + 1
             end if
          end do
          deallocate(time_loc_R8)
       end do
       Xdim=nlon
       Ydim=jx
       if (allocated (time)) then
          deallocate(time)
          allocate (time(Xdim, Ydim))
       end if
       if (allocated (lon)) then
          deallocate(lon)
          allocate (lon(Xdim, Ydim))
       end if
       if (allocated (lat)) then
          deallocate(lat)
          allocate (lat(Xdim, Ydim))
       end if
       !
       !!write(6,'(2x,a,10i10)') 'true  Xdim, Ydim:', Xdim, Ydim
       !!write(6,'(2x,a,10i10)') 'false Xdim, Ydim:', nlon, j2
       !


       !
       ! -- determine true time/lon/lat by filtering T < 0
       !
       jx=0
       do i = 1, M
          filename = filenames(i)
          nlon = nlons(i)
          nlat = nlats(i)
          !!write(6,'(2x,a,10i6)')  'M, i, nlon, nlat:', M, i, nlon, nlat
          !!write(6,'(2x,a)') 'time_loc_r8'
          !
          allocate (time_loc_R8(nlon, nlat))
          call get_var_from_name_w_group (var_name_time, time_loc_R8, filename, _RC)
          allocate (lon_loc(nlon, nlat))
          call get_var_from_name_w_group (var_name_lon, lon_loc, filename, _RC)
          allocate (lat_loc(nlon, nlat))
          call get_var_from_name_w_group (var_name_lat, lat_loc, filename, _RC)
          !
          do j=1, nlat
             !
             ! -- filter, e.g., eliminate -9999
             !
             if ( time_loc_R8(1, j) > 0.0 ) then
                jx = jx + 1
                time(1:nlon,jx) = time_loc_R8(1:nlon,j)
                lon (1:nlon,jx) = lon_loc (1:nlon,j)
                lat (1:nlon,jx) = lat_loc (1:nlon,j)
             end if
             !!write(6,'(5f20.2)') time_loc_R8(1,j)
          end do

          !!write(6,'(2x,a,10i10)') 'end of file id', i
          !!write(6,*)

          deallocate(time_loc_R8)
          deallocate(lon_loc)
          deallocate(lat_loc)
       end do

    else

       jx=0
       do i = 1, M
          filename = filenames(i)
          nlon = nlons(i)
          nlat = nlats(i)

          if (present(var_name_time).AND.present(time)) then
             allocate (time_loc_R8(nlon, nlat))
             call get_var_from_name_w_group (var_name_time, time_loc_R8, filename, _RC)
             time(1:nlon,jx+1:jx+nlat) = time_loc_R8(1:nlon,1:nlat)
             deallocate(time_loc_R8)
          end if

          if (present(var_name_lon).AND.present(lon)) then
             allocate (lon_loc(nlon, nlat))
             call get_var_from_name_w_group (var_name_lon, lon_loc, filename, _RC)
             lon(1:nlon,jx+1:jx+nlat) = lon_loc(1:nlon,1:nlat)
             deallocate(lon_loc)
          end if

          if (present(var_name_lat).AND.present(lat)) then
             allocate (lat_loc(nlon, nlat))
             call get_var_from_name_w_group (var_name_lat, lat_loc, filename, _RC)
             lat(1:nlon,jx+1:jx+nlat) = lat_loc(1:nlon,1:nlat)
             deallocate(lat_loc)
          end if

          jx = jx + nlat
       end do

    end if

    _RETURN(_SUCCESS)
  end subroutine read_M_files_4_swath


  !
  !-- caveat: note call this subr. on head node
  !           because of (bash ls) command therein
  !
  function get_filename_from_template_use_index (obsfile_start_time, obsfile_interval, &
       f_index, file_template, exist, rc) result(filename)
    use Plain_netCDF_Time, only : ESMF_time_to_two_integer
    use MAPL_StringTemplate, only : fill_grads_template
    character(len=ESMF_MAXSTR) :: filename
    type(ESMF_Time), intent(in) :: obsfile_start_time
    type(ESMF_TimeInterval), intent(in) :: obsfile_interval
    character(len=*), intent(in) :: file_template
    integer, intent(in) :: f_index
    logical, intent(out) :: exist
    integer, optional, intent(out) :: rc

    integer :: itime(2)
    integer :: nymd, nhms
    integer :: status
    real(ESMF_KIND_R8) :: dT0_s
    real(ESMF_KIND_R8) :: s
    type(ESMF_TimeInterval) :: dT
    type(ESMF_Time) :: time
    integer :: i, j, u

    character(len=ESMF_MAXSTR) :: file_template_left
    character(len=ESMF_MAXSTR) :: file_template_right
    character(len=ESMF_MAXSTR) :: filename_left
    character(len=ESMF_MAXSTR) :: filename_full
    character(len=ESMF_MAXSTR) :: filename2
    character(len=ESMF_MAXSTR) :: cmd

    call ESMF_TimeIntervalGet(obsfile_interval, s_r8=dT0_s, rc=status)
    s = dT0_s * f_index
    call ESMF_TimeIntervalSet(dT, s_r8=s, rc=status)
    time = obsfile_start_time + dT

    call ESMF_time_to_two_integer(time, itime, _RC)
    nymd = itime(1)
    nhms = itime(2)

    ! parse time info
    !
    call fill_grads_template ( filename, file_template, &
         experiment_id='', nymd=nymd, nhms=nhms, _RC )
    inquire(file= trim(filename), EXIST = exist)

    _RETURN(_SUCCESS)

  end function get_filename_from_template_use_index



  subroutine get_var_from_name_w_group (var_name, var2d, filename, rc)
    character(len=ESMF_MAXSTR), intent(in) :: var_name, filename
    real(ESMF_KIND_R8), intent(inout) :: var2d(:,:)
    integer, optional, intent(out) :: rc

    integer :: i, j
    character(len=ESMF_MAXSTR) :: grp1, grp2
    character(len=ESMF_MAXSTR) :: short_name
    integer :: ncid, ncid1, ncid2, ncid_final, varid
    logical :: found_group
    integer :: status


    i=index(var_name, '/')
    if (i>0) then
       found_group = .true.
       grp1 = var_name(1:i-1)
       j=index(var_name(i+1:), '/')
       if (j>0) then
          grp2=var_name(i+1:i+j-1)
          short_name=var_name(i+j+1:)
       else
          grp2=''
          short_name=var_name(i+1:)
       endif
       i=i+j
    else
       found_group = .false.
       grp1 = ''
       grp2=''
       short_name=var_name
    endif


    ! ncid
    ! ncid1:  grp1
    ! ncid2:  grp2
    !
    call check_nc_status(nf90_open(filename, NF90_NOWRITE, ncid), _RC)
    ncid_final = ncid
    if ( found_group ) then
       call check_nc_status(nf90_inq_ncid(ncid, grp1, ncid1), _RC)
       ncid_final = ncid1
       if (j>0) then
          call check_nc_status(nf90_inq_ncid(ncid1, grp2, ncid2), _RC)
          ncid_final = ncid2
       endif
    else
!!       print*, 'no grp name'
    endif

    call check_nc_status(nf90_inq_varid(ncid_final, short_name, varid), _RC)
!!    write(6,*) 'ncid, short_name, varid', ncid, trim(short_name), varid
    call check_nc_status(nf90_get_var(ncid_final, varid, var2d), _RC)

    call check_nc_status(nf90_close(ncid), _RC)

    _RETURN(_SUCCESS)

  end subroutine get_var_from_name_w_group


  subroutine sort_three_arrays_by_time(U,V,T,rc)
    use MAPL_SortMod
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
    use MAPL_SortMod
    real(ESMF_KIND_R8) :: U(:), V(:), T(:)
    integer :: ID(:)
    integer, optional, intent(out) :: rc

    integer :: i, len
    integer, allocatable :: IA(:)
    integer(ESMF_KIND_I8), allocatable :: IX(:)
    real(ESMF_KIND_R8), allocatable :: X(:)
    integer, allocatable :: NX(:)

    _ASSERT(size(U)==size(V), 'U,V different dimension')
    _ASSERT(size(U)==size(T), 'U,T different dimension')
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


  function copy_platform_nckeys(a, rc)
    type(obs_platform) :: copy_platform_nckeys
    type(obs_platform), intent(in) :: a
    integer, optional, intent(out) :: rc

    copy_platform_nckeys%index_name_x = a%index_name_x
    copy_platform_nckeys%var_name_lon = a%var_name_lon
    copy_platform_nckeys%var_name_lat = a%var_name_lat
    copy_platform_nckeys%var_name_time = a%var_name_time
    copy_platform_nckeys%nentry_name = a%nentry_name
    _RETURN(_SUCCESS)

  end function copy_platform_nckeys


  function union_platform(a, b, rc)
    type(obs_platform) :: union_platform
    type(obs_platform), intent(in)  :: a
    type(obs_platform), intent(in)  :: b
    integer, optional, intent(out) :: rc

    character (len=ESMF_MAXSTR), allocatable :: field_name_loc(:,:)
    integer :: nfield, nentry_name
    integer, allocatable :: tag(:)
    integer :: i, j, k
    integer :: status

    union_platform = copy_platform_nckeys(a, _RC)
    nfield = a%ngeoval + b%ngeoval
    allocate (tag(b%ngeoval))

    tag(:)=1    ! true
    k=nfield
    do j=1, b%ngeoval
       do i=1, a%ngeoval
          if ( trim(b%field_name(1,j)) == trim(a%field_name(1,i)) ) then
             tag(j)=0
          endif
       enddo
       if (tag(j)==0) k=k-1
    enddo
    union_platform%ngeoval=k
    nfield=k
    nentry_name=union_platform%nentry_name
    if ( allocated (union_platform%field_name) ) deallocate(union_platform%field_name)
    allocate(union_platform%field_name(nentry_name, nfield))
    do i=1, a%ngeoval
       union_platform%field_name(:,i) = a%field_name(:,i)
    enddo
    if (nfield>a%ngeoval) then
       k = a%ngeoval
       do j=1, b%ngeoval
          if (tag(j)==1) then
             k = k + 1
             union_platform%field_name(:,k) = b%field_name(:,j)
          end if
       enddo
    end if
    _RETURN(_SUCCESS)

  end function union_platform


end module MAPL_ObsUtilMod

#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_ObsUtilMod
  use ESMF
  use MAPL_ErrorHandlingMod
  use MAPL_KeywordEnforcerMod
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  implicit none
  integer, parameter :: mx_ngeoval = 60

contains

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
    character(len=ESMF_MAXSTR), intent(out) :: filenames(200)
    type(ESMF_TimeInterval), intent(in), optional :: T_offset_in_file_content
    integer, optional, intent(out) :: rc

    type(ESMF_Time) :: T1, Tn
    type(ESMF_Time) :: cT1
    type(ESMF_Time) :: Ts, Te
    type(ESMF_TimeInterval) :: dT1, dT2, dTs, dTe
    type(ESMF_TimeInterval) :: Toff
    real(ESMF_KIND_R8) :: dT0_s, dT1_s, dT2_s
    real(ESMF_KIND_R8) :: s1, s2

    integer :: obsfile_Ts_index, obsfile_Te_index
    integer :: n1, n2
    integer :: i, j
    integer :: status

    !__ s1.  Arithmetic index list based on s,e,interval
    !
    print*, __LINE__, __FILE__
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

    print*, 'ck dT0_s, dT1_s, dT2_s', dT0_s, dT1_s, dT2_s
    print*, '1st n1, n2', n1, n2
    
    
    obsfile_Ts_index = n1
    if ( dT2_s - n2*dT0_s < 1 ) then
       obsfile_Te_index = n2 - 1
    else
       obsfile_Te_index = n2
    end if

    ! put back
    n1 = obsfile_Ts_index
    n2 = obsfile_Te_index

    print*, __LINE__, __FILE__
    print*, '2nd n1, n2', n1, n2
    
    !__ s2.  further test file existence
    !    
    j=0
    do i= n1, n2
       j=j+1
       filenames(j) = get_filename_from_template_use_index &
            (obsfile_start_time, obsfile_interval, &
            i, file_template, rc=rc)
    end do
    

  end subroutine Find_M_files_for_currTime

!
!  subroutine read_M_files ( filenames, Xdim, Y_dim, &
!       lon_name, lat_name, time_name, &
!       lon, lat, time )

  function get_filename_from_template_use_index (obsfile_start_time, obsfile_interval, &
       f_index, file_template, rc) result(filename)
    use Plain_netCDF_Time, only : ESMF_time_to_two_integer
    use  MAPL_StringTemplate, only : fill_grads_template    
    character(len=ESMF_MAXSTR) :: filename
    type(ESMF_Time), intent(in) :: obsfile_start_time
    type(ESMF_TimeInterval), intent(in) :: obsfile_interval
    character(len=*), intent(in) :: file_template
    integer, intent(in) :: f_index
    integer, optional, intent(out) :: rc

    integer :: itime(2)
    integer :: nymd, nhms
    integer :: status
    real(ESMF_KIND_R8) :: dT0_s
    real(ESMF_KIND_R8) :: s
    type(ESMF_TimeInterval) :: dT
    type(ESMF_Time) :: time
    integer :: i, j

    character(len=ESMF_MAXSTR) :: file_template_left
    character(len=ESMF_MAXSTR) :: file_template_right
    character(len=ESMF_MAXSTR) :: filename_left
    character(len=ESMF_MAXSTR) :: filename_full
    character(len=ESMF_MAXSTR) :: cmd

    print*, __LINE__, __FILE__

    call ESMF_TimeIntervalGet(obsfile_interval, s_r8=dT0_s, rc=status)
    s = dT0_s * f_index
    call ESMF_TimeIntervalSet(dT, s_r8=s, rc=status)
    time = obsfile_start_time + dT
    print*, __LINE__, __FILE__

    call ESMF_time_to_two_integer(time, itime, _RC)
    nymd = itime(1)
    nhms = itime(2)

    print*, 'nymd, nhms=', nymd, nhms


    j= index(file_template, '*')
    if (j>0) then
       ! wild char exist
       print*, 'pos of * in template =', j
       file_template_left = file_template(1:j-1)
    else
       file_template_left = file_template       
    endif
    write(6,*) 'file_template_left=', trim(file_template_left)
    
    call fill_grads_template ( filename_left, file_template_left, &
         experiment_id='', nymd=nymd, nhms=nhms, _RC )

    print*, 'new filename_left=', trim(filename_left)

    filename= trim(filename_left)//trim(file_template(j:))

    cmd="bash -c 'ls "//trim(filename)//"' &> zzz_MAPL"
    print*, 'cmd=', trim(cmd)
    CALL execute_command_line(trim(cmd))

    open(7213, file='zzz_MAPL', status='unknown')
    read(7213, '(a)') filename
    print*, 'readin filename=', trim(filename)

    i=index(trim(filename), 'ls')
    if (i==1) then
       filename=''
       write(6,*) 'No such file or directory:', trim(filename_left)//trim(file_template(j:))
    end if
    
    cmd="rm -f ./zzz_MAPL"
    CALL execute_command_line(trim(cmd))
    close(7213)
    
    _RETURN(_SUCCESS)

  end function get_filename_from_template_use_index

end module MAPL_ObsUtilMod



program main
  use ESMF
  use MAPL_ObsUtilMod
  implicit none

    type(ESMF_Time) :: currTime
    type(ESMF_Time) :: obsfile_start_time, obsfile_end_time
    type(ESMF_TimeInterval) :: obsfile_interval, epoch_frequency
    type(ESMF_TimeInterval) :: Toff    
    character(len=ESMF_MAXSTR) :: file_template
    character(len=ESMF_MAXSTR) :: STR1
    character(len=ESMF_MAXSTR) :: filenames(200)
    integer :: M
    integer :: i    
    real(KIND=ESMF_KIND_R8) :: sec
    integer :: rc, status
    type(ESMF_Calendar) :: gregorianCalendar

    file_template = '/Users/yyu11/ModelData/earthData/flk_modis_MOD04_2017_090/MOD04_L2.A%y4%D3.%h2%n2.*.hdf'
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, name='Gregorian_obs' , rc=rc)

        
!    STR1='2017-03-31T00:00:00'
!    call ESMF_TimeSet(currTime, trim(STR1), rc=rc)
         
!    STR1='2017-03-31T00:00:00'
!    call ESMF_TimeSet(obsfile_start_time, trim(STR1), rc=rc)

!    STR1='2017-04-01T00:00:00'
!    call ESMF_TimeSet(obsfile_end_time, trim(STR1), rc=rc)


    call ESMF_TimeSet(currTime, yy=2017, mm=3, dd=31, h=0, m=0, s=0, &
           calendar=gregorianCalendar, rc=rc)
    obsfile_start_time = currTime
    call ESMF_TimeSet(obsfile_end_time, yy=2018, mm=3, dd=31, h=0, m=0, s=0, &
         calendar=gregorianCalendar, rc=rc)
    
    sec = 300.d0
    call ESMF_TimeIntervalSet(obsfile_interval, h=0, m=5, s=0, rc=rc)

    sec = 3600.d0
    call ESMF_TimeIntervalSet(Epoch_frequency, h=1, m=0, s=0, rc=rc)    
    
    sec = 0.d0
    call ESMF_TimeIntervalSet(Toff, s_r8=sec, rc=status)
    
    call Find_M_files_for_currTime (currTime, &
       obsfile_start_time, obsfile_end_time, obsfile_interval, &
       epoch_frequency, file_template, M, filenames, &
       T_offset_in_file_content = Toff,  rc = rc)

    stop -2
    write(6,*) 'M=', M
    do i=1, M
       write(6,*) 'filenames(i)=', trim(filenames(i))
    end do

  end program main

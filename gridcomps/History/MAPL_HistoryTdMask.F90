#include "MAPL_Generic.h"
module MAPL_TimeDependentMaskMod
  use ESMF
  implicit none
  private

  public :: TimeDependentMask
  type :: TimeDependentMask
     logical, allocatable :: mask(:,:)    ! for CS and LL
     character(len=ESMF_MAXPATHLEN) :: mask_file_header
     type(ESMF_Time) :: obs_start
     type(ESMF_Time) :: obs_end
     type(ESMF_TimeInterval) :: obs_interval
     type(ESMF_Time) :: mask_start
     type(ESMF_Time) :: mask_end
     type(ESMF_Time) :: mask_freq
   contains
     procedure :: get_mask
     procedure :: get_filename_arraybound
  end type TimeDependentMask

  interface TimeDependentMask
     procedure new_TimeDependentMask
  end interface TimeDependentMask

  integer :: maxstr=512
  
contains

  function new_TimeDependentMask(mask_setup) result(tdmask)
    ! convert mask start_time, end_time to ESMF time
    ! read in
    type(TimeDependentMask) :: tdmask
    character(len=ESMF_MAXPATHLEN), intent(in) :: mask_setup
    integer :: nx, ny

    character(maxstr) :: ss
    character(maxstr) :: s1, s2, s3, s4   
    type(ESMF_Time) :: startTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Calendar) :: gregorianCalendar
    character(len=maxstr) :: mystring
    character(len=maxstr), allocatable :: string_pieces(:)
    character(len=maxstr), allocatable :: string_pieces2(:)
    integer :: max_len, max_seg, nseg
    integer :: rc, status
    integer :: idoy,ih,im,is    
    integer :: iyy, imm, idd

    call getData_timeinfo(mask_setup, tdmask%obs_start, tdmask%obs_end, &
         tdmask%obs_interval, tdmask%mask_start, tdmask%mask_file_header)

    !!    tdmask%mask(:,:)=.false.

  end function new_TimeDependentMask


  subroutine get_mask(this, time_span, grid, rc)
    class(TimeDependentMask), intent(inout) :: this
    type(ESMF_Time), intent (in) :: time_span(2)
    type(ESMF_grid), intent (in) :: grid  ! CS or LL from model/bundle
    integer, optional, intent(out) :: rc

    integer :: status
    integer, allocatable :: COUNTS(:)
    integer :: IM, JM, LM, IM_WORLD, JM_WORLD

    integer :: nx, nlon
    character(len=ESMF_MAXPATHLEN) :: s1, s2, s3, s4
    type(ESMF_time) :: start_time
    type(ESMF_time) :: start_time_aux

    character(len=ESMF_MAXPATHLEN) :: fname    

    !    
!    ! s1. get esmf grid dim, default mask=.F.
!    call ESMF_GridGet(grid, DistGrid=disgrid, dimCount=dimCount, _RC)
!    call ESMF_DistGridGet(distgrid, deLayout=LAYOUT, _RC)
!    call EMSF_VmGetCurrent(VM, _RC)
!    call ESMF_VmGet(VM, localPet=myid, petCount=ndes, _RC)
!
!    call ESMF_GridGet(grid, localCellCountPerDim=COUNTS, _RC)
!    IM= COUNTS(1)
!    JM= COUNTS(2)
!    LM= COUNTS(3)
!
!    call ESMF_GridGet(grid, globalCellCountPerDim=COUNTS, _RC)
!    IM_WORLD= COUNTS(1)
!    JM_WORLD= CONNTS(2)
!
!    !    allocate(this%mask(IM, JM))
!    !    this%mask(:,:)=.F.

    
    ! s2. read in a series of swath files within time_span
    !     - parse ob filename, dir, freq. from hist-input
    !     - read in lon/lat obs. data
    start_time=this%mask_start
    if (start_time < time_span(1)) then
       do while ( start_time <= time_span(1) )
          start_time = start_time + this%obs_interval
       enddo
    endif
    start_time_aux=start_time
    nx=0
    do while ( start_time <= time_span(2) )
       call this%get_filename_arraybound (start_time, fname, nlon)
       nx=nx+nlon
       write(6,121) 'nx', nx
       write(6,102) 'fname', fname
       start_time=start_time+this%obs_interval
    enddo

    this%mask_start=start_time

    ! s3. find index [loc/global] via bisect for CS/LL
    !
    !

    include "/users/yyu11/sftp/myformat.inc"  
  end subroutine get_mask




  subroutine getData_timeinfo(mask_setup, obs_start, obs_end, obs_interval, mask_start, file_root)
    !
    ! Exact timeinfo from mask_setup
    !
    character(len=ESMF_MAXPATHLEN), intent(in) :: mask_setup
    type(ESMF_Time), intent(out) :: obs_start
    type(ESMF_Time), intent(out) :: obs_end
    type(ESMF_Time), intent(out) :: mask_start    
    type(ESMF_TimeInterval), intent(out) :: obs_interval
    character(len=ESMF_MAXPATHLEN), intent(out) :: file_root    

    type(ESMF_Time) :: startTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Calendar) :: gregorianCalendar
    character(len=maxstr) :: mystring
    character(len=maxstr), allocatable :: string_pieces(:)
    character(len=maxstr), allocatable :: string_pieces2(:)
    integer :: max_len, max_seg, nseg
    integer :: rc, status
    integer :: idoy,ih,im,is    
    integer :: iyy, imm, idd

    max_len=maxstr
    max_seg=100       ! segmane separated by ',' on each line
    allocate(string_pieces(max_seg))
    allocate(string_pieces2(max_seg))

    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, name='Gregorian_obs', rc=rc)

    call split_string(mask_setup,  ' ', max_len, max_seg, nseg, string_pieces, status)
    write(6,*) 'nseg=', nseg
    write(6,*) 'string_pieces(1:nseg)=', string_pieces(1:nseg)
    !
    call split_string(string_pieces(1),'.',max_len, max_seg, nseg, string_pieces2, status)
    read(string_pieces2(1), '(i4,i3)') iyy,idoy
    read(string_pieces2(2), '(i2,i2)') ih,im
    is=0
    write(6,121) 'iyy,idoy,ih,im,is', iyy,idoy,ih,im,is
    !
    call ESMF_timeSet(startTime, yy=iyy,mm=1,dd=1,h=0,m=0,s=0, &
         calendar=gregorianCalendar, rc=rc)
    idoy=idoy-1
    call ESMF_timeintervalSet(timestep,d=idoy, h=ih, m=im, s=is, rc=rc)
    obs_start=startTime+timeStep
    mask_start=obs_start

    call split_string(string_pieces(2),'.',max_len, max_seg, nseg, string_pieces2, status)
    read(string_pieces2(1), '(i4,i3)') iyy,idoy
    read(string_pieces2(2), '(i2,i2)') ih,im
    is=0
    !
    call ESMF_timeSet(startTime, yy=iyy,mm=1,dd=1,h=0,m=0,s=0, &
         calendar=gregorianCalendar, rc=rc)
    idoy=idoy-1
    call ESMF_timeintervalSet(timestep,d=idoy, h=ih, m=im, s=is, rc=rc)
    obs_end=startTime+timeStep


    read(string_pieces(3), '(3i2)') ih, im, is
    write(6,102) 'string_pieces(3) = obs_interval', trim(string_pieces(3))
    write(6, '(3i2)') ih, im, is    

    call ESMF_timeintervalSet(obs_interval, d=0, h=ih, m=im, s=is, rc=rc)    


    ! check
    ! -----
    call ESMF_timeGet(obs_start, yy=iyy, mm=imm, dd=idd, h=ih, m=im, s=is, rc=rc)
    write(6, 121) 'obs_start: iyy,imm,idd,ih,im,is', iyy,imm,idd,ih,im,is
    call ESMF_timeGet(obs_end, yy=iyy, mm=imm, dd=idd, h=ih, m=im, s=is, rc=rc)
    write(6, 121) 'obs_end  : iyy,imm,idd,ih,im,is', iyy,imm,idd,ih,im,is

    
    call split_string(string_pieces(4),'.',max_len, max_seg, nseg, string_pieces2, status)
    read(string_pieces2(1), '(a)') file_root
    file_root=trim(file_root)//'.A'         ! hard-coded format for MODIS swath
    

    include "/users/yyu11/sftp/myformat.inc"  
  end subroutine getData_timeinfo



  subroutine  get_filename_arraybound (this, obs_time, fname, ndim)
    class(TimeDependentMask), intent(in) :: this
    type(ESMF_time), intent(in) :: obs_time
    character(len=ESMF_MAXPATHLEN), intent(out):: fname
    integer, intent(out) :: ndim
    type(ESMF_time) :: ref_time
    type(ESMF_timeInterval) :: timestep
    type(ESMF_Calendar) :: gregorianCalendar
    character(len=ESMF_MAXPATHLEN) :: s
    integer :: rc
    integer :: iyy, imm, idd, idoy
    integer :: ih, im, is
    
!!    time_step = obs_time - this%obs_start
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, name='Gregorian_obs', rc=rc)
    call ESMF_timeGet(obs_time, yy=iyy, mm=imm, dd=idd, h=ih, m=im, s=is, rc=rc)
    call ESMF_timeSet(ref_time, yy=iyy, mm=1,   dd=1,   h=0,  m=0,  s=0, &
         calendar=gregorianCalendar, rc=rc)
    timestep = obs_time - ref_time
    call ESMF_timeIntervalGet(timestep, d=idoy, h=ih, m=im, s=is, rc=rc)
    write(s,'(i4,i0.3,a1,2i0.2,a4)') iyy, idoy+1, '.', ih, im, '.nc4'
    fname=trim(this%mask_file_header)//trim(s)
    write(6,*)  'fname=', fname
    
    ndim=1
    stop -1       
  end subroutine get_filename_arraybound
    

    subroutine split_string (string, mark, length_mx, &
       mxseg, nseg, str_piece, jstatus)
    implicit none
    integer,           intent (in) :: length_mx
    character (len=length_mx), intent (in) :: string
    character (len=1), intent (in) :: mark
    integer,           intent (in) :: mxseg
    integer,           intent (out):: nseg
    character (len=length_mx), intent (out):: str_piece(mxseg)
    integer,           intent (out):: jstatus
    INTEGER                        :: len1, l, lh, lw
    integer                        :: iseg
    integer,           allocatable :: ipos(:)
    !
    !   xxxx_yy_zz_uu_vv
    !       ^  ^  ^  ^
    !       |  |  |  |
    !       marker
    !
    len1 = LEN_TRIM( string )
    allocate (ipos(len1))
    iseg=0
    do l=1, len1
       if (mark .eq. string(l:l)) then
          iseg=iseg+1
          ipos(iseg)=l
  !        write(6,*) 'match!', l
  !        write(6,*) 'ipos ', iseg, ' = ', l
       endif
    enddo
    if (iseg.eq.0 .or. iseg.gt.mxseg-1) then
       call error ('split_string', 'find nseg .eq.0 or > 4', 1)
       jstatus=1   ! fail
       return
    else
       jstatus=0   ! success
    endif
    nseg=iseg
    !
    !
    str_piece(:)=''
    lw=1    ! lw, lh: two index positions
    do l=1, nseg
       lh=ipos(l)-1
       do iseg=lw, lh
          str_piece(l)=trim(str_piece(l))//string(iseg:iseg)
       enddo
       lw=ipos(l)+1
    enddo
    if (lw.le.len1) then
       lh=len1
       do iseg=lw, lh
          str_piece(l)=trim(str_piece(l))//string(iseg:iseg)
       enddo
    endif
    nseg=nseg+1  ! must add one bc of eggs and '_'
    if (nseg.gt.mxseg) then
       call error ('split_string', 'nseg exceeds mx', 1)
    endif
    str_piece(nseg+1:mxseg)='void'
    return
  end subroutine split_string
  

  subroutine error(insubroutine, message, ierr )
    character (len=*), intent (in) :: insubroutine
    character (len=*), intent (in) :: message
    integer, intent (in) :: ierr
    !
    write (6, 11)
    write (6, 12)  trim(insubroutine), trim(message), ierr
    write (6, 11)
    stop
11  format ('**====================**')
12  format (2x, a, 4x, a, 4x, "ierr =", i4)
    return
  end subroutine error

  
end module MAPL_TimeDependentMaskMod



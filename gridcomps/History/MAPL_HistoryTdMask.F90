#include "MAPL_Generic.h"
module MAPL_TimeDependentMaskMod
  use ESMF
  use netcdf
  use mapl_MaplGrid, only : MAPL_GridGet
  use MAPL_Base, only : MAPL_GetHorzIJIndex, MAPL_GetGlobalHorzIJIndex
  use MAPL_ErrorHandlingMod
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

  end function new_TimeDependentMask


  subroutine get_mask(this, time_span, grid, rc)
    implicit none
    class(TimeDependentMask), intent(inout) :: this
    type(ESMF_Time), intent (in) :: time_span(2)
    type(ESMF_grid), intent (inout) :: grid  ! CS or LL from model/bundle
    integer, optional, intent(out) :: rc

    integer :: status
    integer :: IM, JM, LM, IM_WORLD, JM_WORLD, COUNTS(3)
    type(ESMF_DistGrid) :: distGrid
    type(ESMF_DElayout) :: layout
    type(ESMF_VM) :: VM
    integer :: myid
    integer :: ndes
    integer :: dimCount
    
    integer :: Xdim, Ydim, nx, npts
    character(len=ESMF_MAXPATHLEN) :: s1, s2, s3, s4
    type(ESMF_time) :: start_time
    type(ESMF_time) :: start_time_aux
    character(len=ESMF_MAXPATHLEN) :: fname    

    real, allocatable :: obs_lons(:)
    real, allocatable :: obs_lats(:)
    real, allocatable :: lons(:,:)
    real, allocatable :: lats(:,:)
    integer, allocatable :: II(:)
    integer, allocatable :: JJ(:)

    integer :: i,j
    
    ! s1. get esmf grid dim, default mask=.F.
    call ESMF_GridGet(grid, DistGrid=distgrid, dimCount=dimCount, _RC)
    call ESMF_DistGridGet(distgrid, deLayout=LAYOUT, _RC)
    call ESMF_DELayoutGet(layout, VM=vm, _RC)
    call ESMF_VmGet(VM, localPet=myid, petCount=ndes, _RC)
    call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, _RC)
    IM= COUNTS(1)
    JM= COUNTS(2)
    LM= COUNTS(3)
    call MAPL_GridGet(grid, globalCellCountPerDim=COUNTS, _RC)
    IM_WORLD= COUNTS(1)
    JM_WORLD= COUNTS(2)

    write(6,121) 'myid,ndes', myid,ndes
    write(6,121) 'IM,JM,LM', IM,JM,LM
    write(6,121) 'IM_WORLD,JM_WORLD', IM_WORLD,JM_WORLD

    allocate(this%mask(IM_WORLD, JM_WORLD))
    this%mask=.false.

    ! s2. read in a series of swath files within time_span
    !     - parse ob filename, dir, freq. from hist-input
    !     - read in lon/lat obs. data
    !
    start_time = this%mask_start
    if (start_time < time_span(1)) then
       do while ( start_time <= time_span(1) )
          start_time = start_time + this%obs_interval
       enddo
    endif
    start_time_aux=start_time

    ! allocate arrays
    nx=0
    do while ( start_time <= time_span(2) .AND. start_time <= this%obs_end)
       call this%get_filename_arraybound (start_time, fname, Xdim, Ydim)
       nx = nx + Xdim*Ydim
       write(6,121) 'nx increase', Xdim*Ydim
       write(6,102) 'fname', trim(fname)
       start_time = start_time + this%obs_interval
    enddo
    write(6,121) 'nx final:', nx
    allocate(obs_lons(nx), obs_lats(nx))
    allocate(II(nx), JJ(nx))
    
    ! fill in arrays [repeat loop]
    nx=0
    start_time=start_time_aux
    do while ( start_time <= time_span(2) .AND. start_time <= this%obs_end)
       call this%get_filename_arraybound (start_time, fname, Xdim, Ydim)
       if(Xdim>0) then
          allocate(lons(Xdim,Ydim), lats(Xdim,Ydim))
          call get_v2d_netcdf(fname, 'clon', lons, Xdim, Ydim)
          call get_v2d_netcdf(fname, 'clat', lats, Xdim, Ydim)
          npts = Xdim*Ydim
          obs_lons(nx+1:nx+npts) = reshape(lons, [npts])
          obs_lats(nx+1:nx+npts) = reshape(lats, [npts])
          nx = nx + npts
          deallocate(lons, lats)
       endif
       start_time = start_time + this%obs_interval
    enddo
    this%mask_start=start_time
    !! write(6,203) obs_lons(1:nx:100)


    ! s3. find index [loc/global] via bisect for CS/LL
    !
    if (nx >0) then
       call  MAPL_GetGlobalHorzIJIndex(nx,II,JJ,lon=obs_lons,lat=obs_lats,grid=Grid,_RC)
       !call MAPL_GetHorzIJIndex(nx,II,JJ,lon=obs_lons,lat=obs_lats,grid=grid,_RC)
       do i=1, nx
          if ( II(i)>0 .AND. JJ(i)>0 ) then
             this%mask( II(i), JJ(i) ) = .true.
          endif
       enddo
       write(6,123) (II(i), i=1,nx,100)
       write(6,124) ((this%mask(i,j), i=1,IM_WORLD,5), j=1,JM_WORLD,5)
    endif
    stop -1

    deallocate(obs_lons, obs_lats)
    deallocate(II, JJ)
    deallocate(this%mask)

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
    integer :: k
    
    max_len=maxstr
    max_seg=100       ! segmane separated by ',' on each line
    allocate(string_pieces(max_seg))
    allocate(string_pieces2(max_seg))

    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, name='Gregorian_obs', rc=rc)

    call split_string(mask_setup,  ' ', max_len, max_seg, nseg, string_pieces, status)
    write(6,*) 'nseg=', nseg
    write(6,102) 'string_pieces(1:nseg)=', (trim(string_pieces(k)), k=1,nseg)
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



  subroutine  get_filename_arraybound (this, obs_time, fname, Xdim, Ydim)
    class(TimeDependentMask), intent(in) :: this
    type(ESMF_time), intent(in) :: obs_time
    character(len=ESMF_MAXPATHLEN), intent(out):: fname
    integer, intent(out) :: Xdim, Ydim

    type(ESMF_time) :: ref_time
    type(ESMF_timeInterval) :: timestep
    type(ESMF_Calendar) :: gregorianCalendar
    character(len=ESMF_MAXPATHLEN) :: s
    integer :: iyy, imm, idd, idoy
    integer :: ih, im, is
    integer :: ntime
    integer :: rc, status
    
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, name='Gregorian_obs', rc=rc)
    call ESMF_timeGet(obs_time, yy=iyy, mm=imm, dd=idd, h=ih, m=im, s=is, rc=rc)
    call ESMF_timeSet(ref_time, yy=iyy, mm=1,   dd=1,   h=0,  m=0,  s=0, &
         calendar=gregorianCalendar, rc=rc)
    timestep = obs_time - ref_time
    call ESMF_timeIntervalGet(timestep, d=idoy, h=ih, m=im, s=is, rc=rc)
    write(s,'(i4,i0.3,a1,2i0.2,a4)') iyy, idoy+1, '.', ih, im, '.nc4'
    fname=trim(this%mask_file_header)//trim(s)
    call get_ncfile_dimension(fname, Xdim, Ydim, ntime)
  end subroutine get_filename_arraybound
    


  subroutine get_ncfile_dimension(filename, nlon, nlat, tdim)
!    use netcdf
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(out) :: nlat, nlon, tdim
    integer :: ncid , dimid
    integer :: rc, status

    character(len=100) :: lon_str, lat_str
    lon_str="cell_across_swath"
    lat_str="cell_along_swath"


    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), rc=rc)
    if (rc/=0) then
       nlon=0
       nlat=0
       tdim=0
       !write(6,*) 'eror in nf90_open, fileName=', trim(fileName)
       !write(6,*) 'rc=', rc
       return
    endif

    call check_nc_status(nf90_inq_dimid(ncid, "time", dimid), _RC)
    call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=tdim), _RC)
    !
    call check_nc_status(nf90_inq_dimid(ncid, lon_str, dimid), _RC)
    call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=nlon), _RC)
    !
    call check_nc_status(nf90_inq_dimid(ncid, lat_str, dimid), _RC)
    call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=nlat), _RC)
    call check_nc_status(nf90_close(ncid), _RC)
    !! debug summary
    write(6,*) "get_ncfile_dimension:  nlat, nlon, tdim = ", nlat, nlon, tdim
  end subroutine get_ncfile_dimension

  
  subroutine get_v2d_netcdf(filename, name, array, Xdim, Ydim)
    use netcdf
    implicit none
    character(len=*), intent(in) :: name, filename
    integer, intent(in) :: Xdim, Ydim
    real, dimension(Xdim,Ydim), intent(out) :: array
    integer :: ncid, varid
    real    :: scale_factor, add_offset
    integer :: rc, status, iret
    
    call check_nc_status (nf90_open      (trim(fileName), NF90_NOWRITE, ncid), _RC)
    call check_nc_status (nf90_inq_varid (ncid,  name,  varid), _RC)
    call check_nc_status (nf90_get_var   (ncid, varid,  array), _RC)
    
    iret = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    if(iret .eq. 0) array = array * scale_factor
    !
    iret = nf90_get_att(ncid, varid, 'add_offset', add_offset)
    if(iret .eq. 0) array = array + add_offset
    !
    iret = nf90_close(ncid)
  end subroutine get_v2d_netcdf


  subroutine check_nc_status(status, rc)
    implicit none
    integer, intent (in) :: status
    integer, intent (out), optional :: rc
    if(status /= nf90_noerr) then
       print *, 'netCDF error: '//trim(nf90_strerror(status))
    endif
    if(present(rc))  rc=status-nf90_noerr
  end subroutine check_nc_status
  

  
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

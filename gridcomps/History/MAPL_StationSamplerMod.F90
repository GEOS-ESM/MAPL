!
! metacode:
! input:  bundle, station_id_file, station_data_file
!    - bundle: grid, field(x, y, z)
!    - station: [id, name, lat, lon, elevate (Arlindo: no verti-interp)]; static file
!
! core:
!    - filemetadata to store NC specifics
!    - LS route handle
!    - LS interp
!
! output:
!    - write nc
!    - not yet append with time stamp 

#include "MAPL_Generic.h"
module StationSamplerMod
  use ESMF
  use MAPL_ErrorHandlingMod
  !use MAPL_KeywordEnforcerMod
  !use MAPL_FileMetadataUtilsMod
  use LocStreamFactoryMod
  use pFIO
  use MAPL_GriddedIOItemVectorMod
  use MAPL_GriddedIOItemMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_LocstreamRegridderMod
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  private

  public :: StationSampler
  type :: StationSampler
     type(LocStreamFactory) :: LSF   !? redundancy
     type(ESMF_LocStream) :: esmf_ls 
     type(LocstreamRegridder) :: regridder
     integer :: nstation
     integer, allocatable :: station_id(:)
     character(len=ESMF_MAXSTR), allocatable :: station_name(:)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL64), allocatable :: elevs(:)
     !
     type(ESMF_FieldBundle) :: bundle
     type(FileMetadata) :: fmd
     type(NetCDF4_FileFormatter) :: formatter
     integer :: number_written, count_write_times
     character(LEN=ESMF_MAXPATHLEN) :: ncfile ! file_name
   contains
     procedure :: add_metadata_route_handle
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: interp_write_file
     !!procedure :: destroy_sampler  !! destructor __ deallocate arrays
  end type StationSampler

  interface StationSampler
     module procedure new_StationSampler_readfile
  end interface StationSampler
  integer :: maxstr = 2048  ! because ESMF_MAXSTR=256  
contains

  !_ initializer / constructor from file
  !
  function new_StationSampler_readfile (filename1,filename2,rc) result(sampler)
    type(StationSampler) :: sampler
    character(len=*), intent(in) :: filename1, filename2  ! 1:station_name, 2:station_data
    integer, optional, intent(out) :: rc
    ! loc
    character(len=40) :: str
    integer :: max_len, max_seg, nseg
    integer :: unit, ios, nline, id, nstation, status, i
    real :: x, y, z

    !_ 1. read from station_id_file
    !     plain text format: [id,name,lat,lon,elev]

    write(6,*) 'filename1=', trim(filename1)
    open(newunit=unit, file=trim(filename1), form='formatted', access='sequential', status='old')
    ios=0; nstation=0
    do while (ios==0)
       read (unit, *, IOSTAT=ios)  id, str, x, y, z
       if (ios==0) nstation=nstation+1
       !! write(6,*) 'id=',id
    enddo
    !! print*, 'nstation=', nstation
    sampler%nstation=nstation
    allocate(sampler%station_id(nstation))
    allocate(sampler%station_name(nstation))
    allocate(sampler%lons(nstation))
    allocate(sampler%lats(nstation))
    allocate(sampler%elevs(nstation))
    rewind(unit)
    do i=1, nstation
       read(unit, *) sampler%station_id(i), &
            sampler%station_name(i), &
            sampler%lats(i), &
            sampler%lons(i), &
            sampler%elevs(i)
    enddo
    close(unit)
    write(6,*) 'sampler%station_name(1:2) : ', &
         trim(sampler%station_name(1)), ' ', trim(sampler%station_name(2))
    write(6,*) 'sampler%lons(1:2) : ', sampler%lons(1:2)

    
    !_ 2. read from station_data_file
    open(newunit=unit, file=trim(filename2), form='formatted', access='sequential', status='old')

    
    
    
    !_ 2. create LocStreamFactory, then esmf_ls including route_handle
    !
    sampler%LSF = LocStreamFactory(sampler%lons, sampler%lats, _RC)
    sampler%esmf_ls = sampler%LSF%create_locstream(_RC)
    !
    ! init ncfile
    sampler%ncfile=''
    sampler%count_write_times=0
    !
  end function new_StationSampler_readfile

!
!  note-1:
!  var in bundle
!  var (lon, lat, lev) [..., x?? ]
!  gridded __,  un-gridded
!  step by step
!  loop over bundle
!    field
!    dim
!
!
!    Q2: metadata?
!    concept?
!    meta:  variable name, units, ... , dim,  station-id (extra var),
!    when to create metadata?  Initial step, mod as needed,
!    field (lon, lat, lev, time) :  time, unit, longname,  -> meta,
!    filename
!
!    model field, 
!
!    bundle_in: vdata (user: new pressu level) --> bundle_out: vdata
!    add meta
!    do interp
!       output nc
!       

  
  !_ prep fmd, RH
  !
  subroutine add_metadata_route_handle (this,bundle,timeInfo,rc)
    class(StationSampler), intent(inout) :: this
    type(ESMF_FieldBundle), intent(in) :: bundle
    type(TimeData), intent(inout) :: timeInfo   ! why out ?
    integer, optional, intent(out) :: rc   
    !
    integer :: status
    type(ESMF_Grid) :: grid
    !!type(ESMF_Clock) :: clock
    type(variable) :: T, v
    integer :: fieldCount
    integer :: fieldCount_max = 1000
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: var_name, long_name, units, vdims
    integer :: field_rank, i
    logical :: is_present

    
    ! wait
    !    call timeInfo%add_time_to_metadata(this%fmd,_RC)
    !    this%time_info = timeInfo
    !    T = Variable(type=pFIO_REAL32, dimensions='Xdim,Ydim,nf,lev,time')

    this%bundle=bundle
    call this%fmd%add_dimension('nstation',this%nstation)
    call this%fmd%add_dimension('time', 1)  ! time = UNLIMITED
    call this%fmd%add_dimension('lev',  1)  ! lev = 1: aeronet on surface 
    !
    T = Variable(type=pFIO_REAL32, dimensions='nstation')
    call T%add_attribute('long_name','longitude')
    call T%add_attribute('unit','degree_east')    
    call this%fmd%add_variable('longitude',T)
    !
    T = Variable(type=pFIO_REAL32, dimensions='nstation')
    call T%add_attribute('long_name','latitude')
    call T%add_attribute('unit','degree_north')    
    call this%fmd%add_variable('latitude',T)    
    !
    T = Variable(type=pFIO_REAL32, dimensions='time')
    call T%add_attribute('long_name','time')
    ! inquire TimeINFO , clock, current_time, ESMF_time, to output --> string
    call T%add_attribute('unit','minutes since 2000-04-14 22:00:00')
    ! -- unit: second ? 7.5 min:  450 s
    call this%fmd%add_variable('time',T)

    
    !?
    !    call ESMF_ClockGet(      CLOCK, currTime=CURRENTTIME, RC=STATUS)

    
    !-- add field in bundle to filemetadata
    !
    call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
    allocate (fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(bundle, fieldNameList=fieldNameList, _RC)
    do i=1, fieldCount
       var_name=trim(fieldNameList(i))
       call ESMF_FieldBundleGet(bundle,var_name,field=field,_RC)
       call ESMF_FieldGet(field,rank=field_rank,_RC)
       call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=is_present,_RC)
       if ( is_present ) then
          call ESMF_AttributeGet(field, NAME="LONG_NAME",VALUE=long_name, _RC)
       else
          long_name = var_name
       endif
       call ESMF_AttributeGet(field,name="UNITS",isPresent=is_present,_RC)
       if ( is_present ) then
          call ESMF_AttributeGet(field, NAME="UNITS",VALUE=units, _RC)
       else
          units = 'unknown'
       endif
       !
       if (field_rank==2) then
          vdims = "nstation,time"
       else if (field_rank==3) then
          vdims = "lev,nstation,time"  ! check convention
       end if
       v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
       call v%add_attribute('units',trim(units))
       call v%add_attribute('long_name',trim(long_name))
       call v%add_attribute('missing_value',MAPL_UNDEF)
       call v%add_attribute('_FillValue',MAPL_UNDEF)
       call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
       call this%fmd%add_variable(trim(var_name),v,_RC)
    enddo  ! fieldCount
    deallocate (fieldNameList)

    
    !_ 2: locstream route handle
    call ESMF_FieldBundleGet(bundle,grid=grid,_RC)
    this%regridder = LocStreamRegridder(grid,this%esmf_ls,_RC)
  end subroutine add_metadata_route_handle

  
  subroutine interp_write_file(this,current_time,rc)
     class(StationSampler), intent(inout) :: this
     type(ESMF_Time), intent(inout) :: current_time
     integer, optional, intent(out) :: rc
     !
     integer :: status
     type(GriddedIOitemVectorIterator) :: iter
     type(GriddedIOitem), pointer :: item
     type(ESMF_Field) :: src_field,dst_field
     integer :: rank,number_to_write,previous_day,current_day
     real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
     real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
     real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
     integer :: fieldCount
     character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
     character(len=ESMF_MAXSTR) :: xname
     integer :: ub(ESMF_MAXDIM), lb(ESMF_MAXDIM)
     integer :: i

     !!real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)

     if (this%count_write_times==0) then
        ! write lon/lat only once
        call this%formatter%put_var('longitude', this%lons)
        call this%formatter%put_var('latitude', this%lats)
     else
        this%count_write_times=1
     endif
     
     !_ s1. account for ungridded_dim from src to dst, interp
     
     call ESMF_FieldBundleGet(this%bundle, fieldCount=fieldCount, _RC)
     allocate (fieldNameList(fieldCount))
     call ESMF_FieldBundleGet(this%bundle, fieldNameList=fieldNameList, _RC)
     do i=1, fieldCount
        xname=trim(fieldNameList(i))
        call ESMF_FieldBundleGet(this%bundle,xname,field=src_field,_RC)
        call ESMF_FieldGet(src_field,rank=rank,_RC)
        if (rank==2) then
           call ESMF_FieldGet(src_field,farrayptr=p_src_2d,_RC)
           dst_field = ESMF_FieldCreate(this%esmf_ls,name=xname, &
                typekind=ESMF_TYPEKIND_R4,_RC)
           call ESMF_FieldGet(dst_field,farrayptr=p_dst_2d,_RC)
           call this%regridder%regrid(p_src_2d,p_dst_2d,_RC)
           if (mapl_am_i_root()) then
              ! plain
              call this%formatter%put_var(trim(xname),p_dst_2d)
              ! fancy
              !call this%formatter%put_var(trim(xname),p_dst_2d, &
              !     start=[1,this%number_written+1],count=[number_to_write = nstation, 1])
           end if
        else if (rank==3) then
           !!STOP 'grid2LS regridder for rank==3 not implemented'
           call ESMF_FieldGet(src_field,farrayptr=p_src_3d,_RC)
           call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)


!           call ESMF_FieldGet(src_field,farrayptr=p_src_3d,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
           !!if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
           !!   lb(1)=1
           !!   ub(1)=this%vdata%lm
           !!end if
           dst_field = ESMF_FieldCreate(this%esmf_ls,name=xname,&
                typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
           call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,_RC)
           call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)              
           !!if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
           !!   allocate(p_new_lev(size(p_src_3d,1),size(p_src_3d,2),this%vdata%lm),_STAT)
           !!   call this%vdata%regrid_eta_to_pressure(p_src_3d,p_new_lev,_RC)
           !!   call this%regridder%regrid(p_new_lev,p_dst_3d,_RC)
           !!else
           !!   call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)
           !!end if
           if (mapl_am_i_root()) then
              call this%formatter%put_var(xname,p_dst_3d)
              !!call this%formatter%put_var(trim(item%xname),p_dst_3d(interval(1):interval(2),:),&
              !!    start=[this%number_written+1,1],count=[number_to_write,size(p_dst_3d,2)])                 
           end if
        else
           STOP 'grid2LS regridder: rank > 3 not implemented'
        end if
     enddo  !  loop fieldCount
     !!     this%number_written=this%number_written+number_to_write
     !!
   end subroutine interp_write_file

   
   subroutine create_file_handle(this,filename,rc)
     class(StationSampler), intent(inout) :: this
     character(len=*), intent(inout) :: filename
     integer, optional, intent(out) :: rc
     !
     type(variable) :: v
     integer :: status

     this%ncfile = trim(filename)
!     v = this%time_info%define_time_variable(_RC)
!     call this%fmd%modify_variable('time',v,_RC)

     if (mapl_am_I_root()) then
        call this%formatter%create(trim(filename),_RC)
        call this%formatter%write(this%fmd,_RC)
     end if
     this%number_written = 0
   end subroutine create_file_handle

  
  subroutine close_file_handle(this,rc)
    class(StationSampler), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status
    !
    if (trim(this%ncfile) /= '') then
       if (mapl_am_i_root()) then
          write(6,*) 'ncfile=', this%ncfile
          call this%formatter%close(_RC)
       end if
    end if
    write(6,*) 'empty file_name=', this%ncfile
    _RETURN(_SUCCESS)
  end subroutine close_file_handle
  
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
  !
end module StationSamplerMod


! Meta code:
! - station sampler obs 
!   station data = /gpfsm/dnb04/projects/p22/aerosol/data/AERONET.v3/Level2/STATIONS/aeronet_locations_v3_2019_lev2.txt
!   format: Site_Name,Longitude(decimal_degrees),Latitude(decimal_degrees),Elevation(meters),JAN,FEB,MAR,
!   wc line:  349  (-2 header)
!   AOD data = /gpfsm/dnb04/projects/p22/aerosol/data/AERONET.v3/Level2/Y2019/M12/aeronet_v3.aod.20191231.txt
!   wc station wo time : 165
!   grep 2019 aeronet_v3.aod.20191231.txt | cut -d, -f1 | sort -u |wc
!   
!   Focus: 2019/M12  
!          20191201  ---  20191231
!
!  
!   var:
!   max_station=1000
!
!  
!  readin
!  - station id, name, lon/lat/elevation, fake AOD_620nm(time, id)
!  - use ESMF_LocStream
!    - use LocStreamFactoryMod::LocStreamFactory::create_locstream
!          LocStreamFactoryMod::LocStreamFactory_from_arrays
!  - do interpolation
!    from model grid to LocStreamFactory::
!          
!  - 
!
!  
!  Details:
!  - aerostation_data_file
!  - aeronet_data_file =  concatenate 2019/12/01 to  2019/12/32 : aeronet_v3.aod.20191201_20191231.txt
!    format
!    FILE_NAME=aeronet_v3.aod.20191231.txt  + (content) +  repeat
!  
!  - aero_data_top_dir :
!     - subdir_format : Y$YYYY$/M$MM$/aeronet_v3.aod.$YYYYMMDD$.txt
!     - subdir_format : Y'YYYY'/M'MM'/aeronet_v3.aod.'yyyymmdd'.txt
!                     : Y_YYYY_/M_MM_/aeronet_v3.aod._YYYYMMDD_.txt
!
!
!  type :: aerostation
!     id = 1:347
!     name = Tallahassee
!     lon/lat/elevat (degree,degree,meters)
!
!     naerostation=
!     ntime=
!     
!  inquire_name
!
!  Explicit array:
!    aero_station_name(:)
!    longitude(:)
!    latitude(:)
!    elevation(:)
!    AOD_620nm(time, id)
!    Ozone_Dobson(time, id)
!
!
! Take reference from:
!   MAPL_HistoryTrajectoryMod.F90  &  GriddedIO.F90 
!

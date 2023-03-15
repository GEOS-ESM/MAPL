!  Input Data:
!_ 1. station_id_file: static
!     plain text format: [id,name,lat,lon,elev]
!_ 2. station_data_file: time series
!     plain text format: [time, AOD..., id]

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
     type(ESMF_time), allocatable :: times(:) ! nobs
     integer, allocatable :: ids(:)         ! ...
     integer :: nobs
     integer :: nstation
     integer, allocatable :: station_id(:)
     integer, allocatable :: id2rank(:)     
     character(len=ESMF_MAXSTR), allocatable :: station_name(:)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL64), allocatable :: elevs(:)
     !
     type(ESMF_FieldBundle) :: bundle
     type(FileMetadata) :: fmd
     type(NetCDF4_FileFormatter) :: formatter
     type(VerticalData) :: vdata
     logical :: do_vertical_regrid
     type(TimeData) :: time_info
     integer :: previous_index
     type(ESMF_Time) :: previous_time
     integer :: obs_written  ! remove number_written
     character(LEN=ESMF_MAXPATHLEN) :: ofile ! file_name
   contains
     procedure :: add_metadata_route_handle
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: append_file
     procedure :: get_file_start_time
     procedure :: reset_times_to_current_day
     procedure :: get_current_interval
     procedure :: compute_times_for_interval
     !!procedure :: destroy_sampler  !! destructor __ deallocate arrays
  end type StationSampler

  interface StationSampler
     module procedure new_StationSampler_readfile
  end interface StationSampler
  integer :: maxstr = 2048  ! because ESMF_MAXSTR=256  
  integer :: maxnstation = 10000 
contains


  function new_StationSampler_readfile (filename1,filename2,rc) result(sampler)
    type(StationSampler) :: sampler
    character(len=*), intent(in) :: filename1, filename2  ! 1:station_name, 2:station_data
    integer, optional, intent(out) :: rc
    ! loc
    character(len=40) :: str, sdmy, shms
    integer :: max_len, max_seg, nseg
    integer :: unit, ios, nline, id, nstation, status, i, j, nobs
    integer :: iday, imonth, iyear, ihr, imin, isec
    real :: x, y, z, t
    character(len=1) :: s1
    
    !_ 1. read from station_id_file: static
    !     plain text format: [id,name,lat,lon,elev]
    !
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
    allocate(sampler%id2rank(maxnstation))
    do i=1, nstation
       sampler%id2rank(sampler%station_id(i))=i
    enddo

    
    write(6,*) 'sampler%station_name(1:2) : ', &
         trim(sampler%station_name(1)), ' ', trim(sampler%station_name(2))
    write(6,*) 'sampler%lons(1:2) : ', sampler%lons(1:2)

    
    !_ 2. read from station_data_file: time series
    open(newunit=unit, file=trim(filename2), form='formatted', access='sequential', status='old')
    ios=0; i=0
    do while (ios==0)
       read (unit, *, IOSTAT=ios) str,str,str,j,t,id,str
       if (ios==0) i=i+1
       !! write(6,*) 'id=',id
    enddo
    nobs=i; sampler%nobs=nobs
    allocate(sampler%times(nobs))
    allocate(sampler%ids(nobs))
    rewind(unit)
    do i=1, nobs
       read(unit, *) str, sdmy, shms, j, t, sampler%ids(i)
       read(sdmy,'(i2,a,i2,a,i4)') iday, s1, imonth, s1, iyear
       read(shms,'(i2,a,i2,a,i2)') ihr, s1, imin, s1, isec
       if (mod(i,100)==1) then
          print*, str, sdmy, shms, j, t, sampler%ids(i)
          print*, iday, imonth, iyear
          print*, ihr, imin, isec
       endif
       call ESMF_TimeSet(sampler%times(i),yy=iyear,mm=imonth,dd=iday,h=ihr,m=imin,s=isec,_RC)
    enddo
    close(unit)
    write(6,*) 'sampler%station_name(1:2) : ', &
         trim(sampler%station_name(1)), ' ', trim(sampler%station_name(2))
    write(6,*) 'sampler%lons(1:2) : ', sampler%lons(1:2)

    
    !_ 3. create LocStreamFactory, then esmf_ls including route_handle
    !
    sampler%LSF = LocStreamFactory(sampler%lons, sampler%lats, _RC)
    sampler%esmf_ls = sampler%LSF%create_locstream(_RC)
    !
    ! init ofile
    sampler%ofile=''
    sampler%obs_written=0
    !
  end function new_StationSampler_readfile

  
  subroutine add_metadata_route_handle (this,bundle,timeInfo,vdata,rc)
    class(StationSampler), intent(inout) :: this
    type(ESMF_FieldBundle), intent(in) :: bundle
    type(TimeData), intent(inout) :: timeInfo
    type(VerticalData), optional, intent(inout) :: vdata
    integer, optional, intent(out) :: rc   
    !
    integer :: status
    type(ESMF_Grid) :: grid
    type(ESMF_Clock) :: clock
    type(variable) :: v
    integer :: fieldCount
    integer :: fieldCount_max = 1000
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: var_name, long_name, units, vdims
    integer :: field_rank, i
    logical :: is_present

    this%bundle=bundle    
    if (present(vdata)) then
       this%vdata=vdata
    else
       this%vdata=VerticalData(_RC)
    end if
    call this%vdata%append_vertical_metadata(this%fmd,this%bundle,_RC)
    this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
    if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

    call timeInfo%add_time_to_metadata(this%fmd,_RC)
    this%time_info = timeInfo
    
    !call this%fmd%add_dimension('nstation',this%nstation)
    !call this%fmd%add_dimension('time', 1)  ! time = UNLIMITED
    !call this%fmd%add_dimension('lev',  1)  ! lev = 1: aeronet on surface 
    !
    v = Variable(type=pFIO_REAL32, dimensions='time')
    call v%add_attribute('long_name','longitude')
    call v%add_attribute('unit','degree_east')    
    call this%fmd%add_variable('longitude',v)
    v = Variable(type=pFIO_REAL32, dimensions='time')
    call v%add_attribute('long_name','latitude')
    call v%add_attribute('unit','degree_north')    
    call this%fmd%add_variable('latitude',v)
    
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
          vdims = "time"
       else if (field_rank==3) then
          vdims = "time,lev"    ! how is lev defined in fmd
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

    ! locstream route handle
    call ESMF_FieldBundleGet(bundle,grid=grid,_RC)
    this%regridder = LocStreamRegridder(grid,this%esmf_ls,_RC)

    this%obs_written=0
    this%previous_index = lbound(this%times,1)-1
    call timeInfo%get(clock=clock,_RC)
    call ESMF_ClockGet(clock,currTime=this%previous_time,_RC)
    
  end subroutine add_metadata_route_handle

  
  subroutine append_file(this,current_time,rc)
     class(StationSampler), intent(inout) :: this
     type(ESMF_Time), intent(in) :: current_time
     integer, optional, intent(out) :: rc
     !
     integer :: status
     type(GriddedIOitemVectorIterator) :: iter
     type(GriddedIOitem), pointer :: item
     type(ESMF_Field) :: src_field,dst_field
     integer :: rank,interval(2),number_to_write,previous_day,current_day
     real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
     real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
     real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
     integer :: fieldCount
     character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
     character(len=ESMF_MAXSTR) :: xname
     integer :: ub(ESMF_MAXDIM), lb(ESMF_MAXDIM)
     real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)
     integer :: i, id, iobs, ix


     interval = this%get_current_interval(current_time)
     if (all(interval==0)) then
        number_to_write = 0
     else
        number_to_write = interval(2)-interval(1)+1
     end if
     if (number_to_write>0) then
        !_ s1. put_var: time/gridded variables
        rtimes = this%compute_times_for_interval(interval,_RC)
        if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
           call this%vdata%setup_eta_to_pressure(_RC)
        end if
        if (mapl_am_i_root()) then
           call this%formatter%put_var('time',rtimes,&
                start=[this%obs_written+1],count=[number_to_write],_RC)
           !!call this%formatter%put_var('longitude',this%lons(interval(1):interval(2)),&
           !!     start=[this%obs_written+1],count=[number_to_write],_RC)
           !!call this%formatter%put_var('latitude',this%lats(interval(1):interval(2)),&
           !!     start=[this%obs_written+1],count=[number_to_write],_RC)
           do iobs=interval(1), interval(2)
              ix=this%id2rank(this%ids(iobs))
              call this%formatter%put_var('longitude',this%lons(ix:ix),&
                   start=[this%obs_written+iobs],count=[1],_RC)
              call this%formatter%put_var('latitude',this%lats(ix:ix),&
                   start=[this%obs_written+iobs],count=[1],_RC)
           enddo
        end if
        deallocate(rtimes)


        !_ s2. put_var: ungridded_dim from src to dst [interpolation]
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
                 !!call this%formatter%put_var(trim(xname),p_dst_2d(interval(1):interval(2)),&
                 !!     start=[this%obs_written+1],count=[number_to_write])
                 do iobs=interval(1), interval(2)
                    ix=this%id2rank(this%ids(iobs))
                    write(6,'(2x,a,2x,2i5)') 'id, ix=', this%ids(iobs), ix
                    call this%formatter%put_var(xname,p_dst_2d(ix:ix),&
                         start=[this%obs_written+iobs],count=[1],_RC)                         
                 enddo
              end if
           else if (rank==3) then
              !!STOP 'grid2LS regridder for rank==3 not implemented'
              call ESMF_FieldGet(src_field,farrayptr=p_src_3d,_RC)
              call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
              !              call ESMF_FieldGet(src_field,farrayptr=p_src_3d,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
              if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
                 lb(1)=1
                 ub(1)=this%vdata%lm
              end if
              dst_field = ESMF_FieldCreate(this%esmf_ls,name=xname,&
                   typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
              call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,_RC)
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 allocate(p_new_lev(size(p_src_3d,1),size(p_src_3d,2),this%vdata%lm),_STAT)
                 call this%vdata%regrid_eta_to_pressure(p_src_3d,p_new_lev,_RC)
                 call this%regridder%regrid(p_new_lev,p_dst_3d,_RC)
              else
                 call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)
              end if
              if (mapl_am_i_root()) then
                 do iobs=interval(1), interval(2)
                    id=this%id2rank(this%ids(iobs))
                    call this%formatter%put_var(xname,p_dst_3d(id:id,1:size(p_dst_3d,2)),&
                         start=[this%obs_written+iobs,1],count=[1,size(p_dst_3d,2)],_RC)
                    !                                   lev            nlev
                 enddo
                 !!call this%formatter%put_var(trim(item%xname),p_dst_3d(interval(1):interval(2),:),&
                 !!    start=[this%obs_written+1,1],count=[number_to_write,size(p_dst_3d,2)])                 
              end if
           else
              STOP 'grid2LS regridder: rank > 3 not implemented'
           end if  ! rank
        enddo  !  loop fieldCount
        this%obs_written=this%obs_written+number_to_write
     endif     !  if number_to_write > 0

     
     !? call ESMF_TimeGet(this%previous_time,dd=previous_day,_RC)
     !? call ESMF_TimeGet(current_time,dd=current_day,_RC)
     !!if (this%recycle_track .and. (current_day/=previous_day)) then
     !!   call this%reset_times_to_current_day(_RC)
     !!   this%previous_index = lbound(this%times,1)-1
     !!end if
     this%previous_time=current_time

   end subroutine append_file
   
   subroutine create_file_handle(this,filename,rc)
     class(StationSampler), intent(inout) :: this
     character(len=*), intent(inout) :: filename
     integer, optional, intent(out) :: rc
     !
     type(variable) :: v
     integer :: status

     this%ofile = trim(filename)
!     v = this%time_info%define_time_variable(_RC)
!     call this%fmd%modify_variable('time',v,_RC)

     if (mapl_am_I_root()) then
        call this%formatter%create(trim(filename),_RC)
        call this%formatter%write(this%fmd,_RC)
     end if
     this%obs_written = 0
   end subroutine create_file_handle

  
  subroutine close_file_handle(this,rc)
    class(StationSampler), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status
    !
    if (trim(this%ofile) /= '') then
       if (mapl_am_i_root()) then
          write(6,*) 'ofile=', this%ofile
          call this%formatter%close(_RC)
       end if
    end if
    write(6,*) 'empty output file_name=', this%ofile
    _RETURN(_SUCCESS)
  end subroutine close_file_handle


  !-- below is a copy from MAPL_HistoryTrajectoryMod.F90
  !
  ! o ---/// ----------- /// ------->
  !            X   X  X
  !            LB     UB  :  interval(1:2)
  ! |--------->
  ! |-------------> rtimes(:)
  !
  function get_current_interval(this,current_time,rc) result(interval)
    class(StationSampler), intent(inout) :: this
    type(ESMF_Time), intent(in) :: current_time
    integer, optional, intent(out) :: rc
    integer :: interval(2)
    integer :: i,nfound
    logical :: found

    found = .false.
    nfound = 0
    interval = 0
    do i=this%previous_index+1,size(this%times)
       if (this%times(i) .ge. this%previous_time .and. this%times(i) .le. current_time) then
          if (.not.found) then
             interval(1) = i
             found = .true.
          end if
          nfound = nfound + 1
       end if
       if (this%times(i) .ge. current_time) exit
    enddo
    if (found) then
       interval(2) = interval(1)+nfound-1
       this%previous_index = interval(2)
    end if
    _RETURN(_SUCCESS)
  end function get_current_interval


  function compute_times_for_interval(this,interval,rc) result(rtimes)
    class(StationSampler), intent(inout) :: this
    integer, intent(in) :: interval(2)
    integer, optional, intent(out) :: rc
    real(ESMF_KIND_R8), allocatable :: rtimes(:)
    integer :: ntimes,i,status,icnt
    type(ESMF_TimeInterval) :: tint
    type(ESMF_Time) :: file_start_time
    character(len=ESMF_MAXSTR) :: tunits
    
    ntimes = interval(2)-interval(1)+1
    if (all(interval==0)) then
       _RETURN(_SUCCESS)
    end if
    call this%get_file_start_time(file_start_time,tunits,_RC)
    allocate(rtimes(ntimes),_STAT)
    icnt=0
    do i=interval(1),interval(2)
       icnt=icnt+1
       tint = this%times(i)-file_start_time
       select case(trim(tunits))
       case ('days')
          call ESMF_TimeIntervalGet(tint,d_r8=rtimes(icnt),_RC)
       case ('hours')
          call ESMF_TimeIntervalGet(tint,h_r8=rtimes(icnt),_RC)
       case ('minutes')
          call ESMF_TimeIntervalGet(tint,m_r8=rtimes(icnt),_RC)
       end select
    enddo
    _RETURN(_SUCCESS)
  end function compute_times_for_interval
    

  subroutine get_file_start_time(this,start_time,time_units,rc)
    class(StationSampler), intent(inout) :: this
    type(ESMF_Time), intent(inout) :: start_time
    character(len=*), intent(inout) :: time_units
    integer, optional, intent(out) :: rc
    
    integer :: status
    class(Variable), pointer :: var
    type(Attribute), pointer :: attr
    class(*), pointer :: pTimeUnits
    character(len=ESMF_MAXSTR) :: timeUnits
    
    integer ypos(2), mpos(2), dpos(2), hpos(2), spos(2)
    integer strlen
    integer firstdash, lastdash
    integer firstcolon, lastcolon
    integer lastspace,since_pos
    integer year,month,day,hour,min,sec
    
    var => this%fmd%get_variable('time',_RC)
    attr => var%get_attribute('units')
    ptimeUnits => attr%get_value()
    select type(pTimeUnits)
    type is (character(*))
       timeUnits = pTimeUnits
       strlen = LEN_TRIM (TimeUnits)
       
       since_pos = index(TimeUnits, 'since')
       time_units = trim(TimeUnits(:since_pos-1))
       time_units = trim(time_units)
       
       firstdash = index(TimeUnits, '-')
       lastdash  = index(TimeUnits, '-', BACK=.TRUE.)
       
       if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
          rc = -1
          return
       endif
       ypos(2) = firstdash - 1
       mpos(1) = firstdash + 1
       ypos(1) = ypos(2) - 3
       
       mpos(2) = lastdash - 1
       dpos(1) = lastdash + 1
       dpos(2) = dpos(1) + 1
       
       read ( TimeUnits(ypos(1):ypos(2)), * ) year
       read ( TimeUnits(mpos(1):mpos(2)), * ) month
       read ( TimeUnits(dpos(1):dpos(2)), * ) day
       
       firstcolon = index(TimeUnits, ':')
       if (firstcolon .LE. 0) then
          
          ! If no colons, check for hour.
          
          ! Logic below assumes a null character or something else is after the hour
          ! if we do not find a null character add one so that it correctly parses time
          if (TimeUnits(strlen:strlen) /= char(0)) then
             TimeUnits = trim(TimeUnits)//char(0)
             strlen=len_trim(TimeUnits)
          endif
          lastspace = index(TRIM(TimeUnits), ' ', BACK=.TRUE.)
          if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
             hpos(1) = lastspace+1
             hpos(2) = strlen-1
             read (TimeUnits(hpos(1):hpos(2)), * ) hour
             min  = 0
             sec  = 0
          else
             hour = 0
             min  = 0
             sec  = 0
          endif
          
       else
          hpos(1) = firstcolon - 2
          hpos(2) = firstcolon - 1
          lastcolon =  index(TimeUnits, ':', BACK=.TRUE.)
          if ( lastcolon .EQ. firstcolon ) then
             mpos(1) = firstcolon + 1
             mpos(2) = firstcolon + 2
             read (TimeUnits(hpos(1):hpos(2)), * ) hour
             read (TimeUnits(mpos(1):mpos(2)), * ) min
             sec = 0
          else
             mpos(1) = firstcolon + 1
             mpos(2) = lastcolon - 1
             spos(1) = lastcolon + 1
             spos(2) = lastcolon + 2
             read (TimeUnits(hpos(1):hpos(2)), * ) hour
             read (TimeUnits(mpos(1):mpos(2)), * ) min
             read (TimeUnits(spos(1):spos(2)), * ) sec
          endif
       endif
    class default
       _FAIL("Time unit must be character")
    end select
    call ESMF_TimeSet(start_time,yy=year,mm=month,dd=day,h=hour,m=min,s=sec,_RC)
    _RETURN(_SUCCESS)
    
  end subroutine get_file_start_time

      
  subroutine get(this, file_name, rc)
    class(StationSampler), intent(inout) :: this
    character(len=*), intent(inout), optional :: file_name
    integer, intent(out), optional :: rc
    
    if (present(file_name)) file_name = trim(this%ofile)
    _RETURN(_SUCCESS)
  end subroutine get

  
  subroutine reset_times_to_current_day(this,rc)
    class(StationSampler), intent(Inout) :: this
    integer, intent(out), optional :: rc
    
    integer :: i,status,h,m,yp,mp,dp,s,ms,us,ns
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: current_time
    integer :: year,month,day
    
    call this%time_info%get(clock=clock,_RC)
    call ESMF_ClockGet(clock,currtime=current_time,_RC)
    call ESMF_TimeGet(current_time,yy=year,mm=month,dd=day,_RC)
    do i=1,size(this%times)
       call ESMF_TimeGet(this%times(i),yy=yp,mm=mp,dd=dp,h=h,m=m,s=s,ms=ms,us=us,ns=ns,_RC)
       call ESMF_TimeSet(this%times(i),yy=year,mm=month,dd=day,h=h,m=m,s=s,ms=ms,us=us,ns=ns,_RC)
    enddo
    
  end subroutine reset_times_to_current_day
  
  
end module StationSamplerMod

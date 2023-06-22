#include "MAPL_Generic.h"
module StationSamplerMod
  use ESMF
  use MAPL_ErrorHandlingMod
  use LocStreamFactoryMod
  use pFIO
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_LocstreamRegridderMod
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use, intrinsic :: iso_c_binding,   only: C_NULL_CHAR
  implicit none
  private

  public :: StationSampler
  type :: StationSampler
     private
     type(LocStreamFactory)   :: LSF
     type(ESMF_LocStream)     :: esmf_ls
     type(LocstreamRegridder) :: regridder
     integer                  :: nstation
     integer, allocatable :: station_id(:)
     character(len=ESMF_MAXSTR), allocatable :: station_name(:)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL64), allocatable :: elevs(:)
     type(ESMF_FieldBundle)         :: bundle
     type(FileMetadata)             :: fmd
     type(NetCDF4_FileFormatter)    :: formatter
     type(VerticalData)             :: vdata
     type(TimeData)                 :: time_info
     character(LEN=ESMF_MAXPATHLEN) :: ofile
     integer                        :: obs_written
   contains
     procedure                      :: add_metadata_route_handle
     procedure                      :: create_file_handle
     procedure                      :: close_file_handle
     procedure                      :: append_file
     procedure                      :: get_file_start_time
     procedure                      :: compute_time_for_current
  end type StationSampler

  interface StationSampler
     module procedure new_StationSampler_readfile
  end interface StationSampler

contains

  function new_StationSampler_readfile (filename,rc) result(sampler)
    use pflogger, only             :  Logger, logging
    implicit none
    type(StationSampler)           :: sampler
    character(len=*), intent(in)   :: filename
    integer, optional, intent(out) :: rc

    character(len=40) :: str, sdmy, shms
    integer :: unit, ios, nstation, status
    integer :: i, j, id, ncount
    real    :: x, y, z
    logical :: con1, con2
    character (len=1)     :: CH1
    character (len=5)     :: seq
    character (len=100)   :: line
    type(Logger), pointer :: lgr

    !__ 1. read from station_id_file: static
    !      plain text format:
    !      [name,lat,lon,elev] or [id,name,lat,lon,elev]
    !
    open(newunit=unit, file=trim(filename), form='formatted', &
         access='sequential', status='old', _IOSTAT)
    ios=0
    nstation=0
    read(unit, '(a100)', IOSTAT=ios) line
    call count_substring(line, ',', ncount)
    con1= ncount.GE.3 .AND. ncount.LE.4
    _ASSERT(con1, 'string sequence in Aeronet file not supported')
    if (ncount==3) then
       seq='AFFF'
    elseif (ncount==4) then
       CH1=line(1:1)
       con1= (CH1>='a'.AND.CH1<='z').OR.(CH1>='A'.AND.CH1<='Z')
       con2= CH1>='0'.AND.CH1<='9'
       if (con1) then
          seq='AIFFF'
       else
          if (con2) then
             seq='IAFFF'
          else
             _ASSERT(.false., 'string sequence in Aeronet file not supported')
          end if
       end if
    end if

    rewind(unit)
    ios=0
    do while (ios==0)
       read(unit, '(a100)', IOSTAT=ios) line
       if (ios==0) nstation=nstation+1
    end do
    sampler%nstation=nstation
    allocate(sampler%station_id(nstation))
    allocate(sampler%station_name(nstation))
    allocate(sampler%lons(nstation))
    allocate(sampler%lats(nstation))
    allocate(sampler%elevs(nstation))
    rewind(unit)
    do i=1, nstation
       if(seq=='IAFFF') then
          read(unit, *) &
               sampler%station_id(i), &
               sampler%station_name(i), &
               sampler%lats(i), &
               sampler%lons(i)
       elseif(seq=='AIFFF') then
          read(unit, *) &
               sampler%station_name(i), &
               sampler%station_id(i), &
               sampler%lats(i), &
               sampler%lons(i)
       elseif(trim(seq)=='AFFF') then
          read(unit, *) &
               sampler%station_name(i), &
               sampler%lats(i), &
               sampler%lons(i)
               sampler%station_id(i)=i
       end if
    end do
    close(unit)
    lgr => logging%get_logger('HISTORY.sampler')
    call lgr%debug('%a %i8',   'nstation=', nstation)
    call lgr%debug('%a %a %a', 'sampler%station_name(1:2) : ', &
         trim(sampler%station_name(1)), trim(sampler%station_name(2)))
    call lgr%debug('%a %f8.2 %f8.2', 'sampler%lons(1:2) : ',&
         sampler%lons(1),sampler%lons(2))
    call lgr%debug('%a %f8.2 %f8.2', 'sampler%lats(1:2) : ',&
         sampler%lats(1),sampler%lats(2))

    !__ 2. create LocStreamFactory, then esmf_ls including route_handle
    !
    sampler%LSF     = LocStreamFactory(sampler%lons, sampler%lats, _RC)
    sampler%esmf_ls = sampler%LSF%create_locstream(_RC)
    !
    ! init ofile
    sampler%ofile=''
    sampler%obs_written=0

    _RETURN(_SUCCESS)
  end function new_StationSampler_readfile

  subroutine add_metadata_route_handle (this,bundle,timeInfo,vdata,rc)
    class(StationSampler),  intent(inout)       :: this
    type(ESMF_FieldBundle), intent(in)          :: bundle
    type(TimeData),         intent(inout)       :: timeInfo
    type(VerticalData), optional, intent(inout) :: vdata
    integer, optional, intent(out)              :: rc

    type(variable)   :: v
    type(ESMF_Grid)  :: grid
    type(ESMF_Clock) :: clock
    type(ESMF_Field) :: field
    integer          :: fieldCount
    integer          :: fieldCount_max = 1000
    integer          :: field_rank
    integer          :: nstation
    logical          :: is_present
    integer          :: ub(ESMF_MAXDIM)
    integer          :: lb(ESMF_MAXDIM)
    logical          :: do_vertical_regrid
    integer          :: status
    integer          :: i

    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: var_name, long_name, units, vdims

    type(ESMF_Info) :: infoh

    !__ 1. metadata add_dimension,
    !     add_variable for time, latlon, station
    !
    this%bundle = bundle
    nstation = this%nstation
    if (present(vdata)) then
       this%vdata = vdata
    else
       this%vdata = VerticalData(_RC)
    end if
    call this%vdata%append_vertical_metadata(this%fmd,this%bundle,_RC) ! specify lev in fmd
    do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
    if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%get_interpolating_variable(this%bundle,_RC)
    endif

    call timeInfo%add_time_to_metadata(this%fmd,_RC) ! specify time in fmd
    this%time_info = timeInfo

    call this%fmd%add_dimension('station_index',nstation)

    v = Variable(type=pFIO_REAL32, dimensions='station_index')
    call v%add_attribute('long_name','longitude')
    call v%add_attribute('unit','degree_east')
    call this%fmd%add_variable('longitude',v)

    v = Variable(type=pFIO_REAL32, dimensions='station_index')
    call v%add_attribute('long_name','latitude')
    call v%add_attribute('unit','degree_north')
    call this%fmd%add_variable('latitude',v)

    v = Variable(type=pFIO_INT32, dimensions='station_index')
    call this%fmd%add_variable('station_id',v)

    !__ 2. filemetadata: extract field from bundle, add_variable
    !
    call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
    allocate (fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(bundle, fieldNameList=fieldNameList, _RC)
    do i=1, fieldCount
       var_name=trim(fieldNameList(i))
       call ESMF_FieldBundleGet(bundle,var_name,field=field,_RC)
       call ESMF_FieldGet(field,rank=field_rank,_RC)
       call ESMF_InfoGetFromHost(field,infoh,_RC)
       is_present = ESMF_InfoIsPresent(infoh, 'LONG_NAME',_RC)
       if ( is_present ) then
          call ESMF_InfoGet(infoh, KEY="LONG_NAME",VALUE=long_name, _RC)
       else
          long_name = var_name
       endif
       is_present = ESMF_InfoIsPresent(infoh, 'UNITS',_RC)
       if ( is_present ) then
          call ESMF_InfoGet(infoh, KEY="UNITS",VALUE=units, _RC)
       else
          units = 'unknown'
       endif
       if (field_rank==2) then
          vdims = "station_index,time"
          v = variable(type=PFIO_REAL32,dimensions=trim(vdims),chunksizes=[nstation,1])
       else if (field_rank==3) then
          vdims = "lev,station_index,time"
          call ESMF_FieldGet(field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
          v = variable(type=PFIO_REAL32,dimensions=trim(vdims),chunksizes=[ub(1)-lb(1)+1,1,1])
       end if
       call v%add_attribute('units',         trim(units))
       call v%add_attribute('long_name',     trim(long_name))
       call v%add_attribute('missing_value', MAPL_UNDEF)
       call v%add_attribute('_FillValue',    MAPL_UNDEF)
       call v%add_attribute('valid_range',   (/-MAPL_UNDEF,MAPL_UNDEF/))
       call this%fmd%add_variable(trim(var_name),v,_RC)
    end do
    deallocate (fieldNameList)

    !__ 3. locstream route handle
    !
    call ESMF_FieldBundleGet(bundle,grid=grid,_RC)
    this%regridder = LocStreamRegridder(grid,this%esmf_ls,_RC)

    _RETURN(_SUCCESS)
  end subroutine add_metadata_route_handle

  subroutine append_file(this,current_time,rc)
    class(StationSampler), intent(inout) :: this
    type(ESMF_Time), intent(in) :: current_time
    integer, optional, intent(out) :: rc
    !
    integer :: status
    integer :: fieldCount
    integer :: ub(1), lb(1)
    type(ESMF_Field) :: src_field,dst_field
    real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
    real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
    real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
    real(kind=REAL32), allocatable :: arr(:,:)
    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: xname
    real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)
    integer :: i, id, iobs, ix, rank
    integer :: nx, nz

    this%obs_written=this%obs_written+1

    !__ 1. put_var: time variable
    !
    rtimes = this%compute_time_for_current(current_time,_RC) ! rtimes: seconds since opening file
    if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%setup_eta_to_pressure(_RC)
    end if
    if (mapl_am_i_root()) then
       call this%formatter%put_var('time',rtimes(1:1),&
            start=[this%obs_written],count=[1],_RC)
    end if

    !__ 2. put_var: ungridded_dim from src to dst [regrid]
    !
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
             call this%formatter%put_var(xname,p_dst_2d,&
                  start=[1,this%obs_written],count=[this%nstation,1],_RC)
          end if
          call ESMF_FieldDestroy(dst_field,nogarbage=.true.)
       else if (rank==3) then
          call ESMF_FieldGet(src_field,farrayptr=p_src_3d,_RC)
          call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
          if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
             lb(1)=1
             ub(1)=this%vdata%lm
          end if
          dst_field = ESMF_FieldCreate(this%esmf_ls,name=xname,&
               typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
          call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,_RC)
          call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)
          if (mapl_am_i_root()) then
             nx=size(p_dst_3d,1); nz=size(p_dst_3d,2); allocate(arr(nz, nx))
             arr=reshape(p_dst_3d,[nz,nx],order=[2,1])
             call this%formatter%put_var(xname,arr,&
                  start=[1,1,this%obs_written],count=[nz,nx,1],_RC)
             !note:     lev,station,time
             deallocate(arr)
          end if
          call ESMF_FieldDestroy(dst_field,nogarbage=.true.)
       else
          _FAIL('grid2LS regridder: rank > 3 not implemented')
       end if
    end do
    deallocate (fieldNameList)
    _RETURN(_SUCCESS)
  end subroutine append_file


  subroutine create_file_handle(this,filename,rc)
    class(StationSampler), intent(inout) :: this
    character(len=*), intent(inout) :: filename  ! for ouput nc
    integer, optional, intent(out) :: rc
    type(variable) :: v
    integer :: status

    this%ofile = trim(filename)
    v = this%time_info%define_time_variable(_RC)
    call this%fmd%modify_variable('time',v,_RC)
    this%obs_written = 0

    if (.not. mapl_am_I_root()) then
       _RETURN(_SUCCESS)
    end if
    call this%formatter%create(trim(filename),_RC)
    call this%formatter%write(this%fmd,_RC)
    call this%formatter%put_var('longitude',this%lons,_RC)
    call this%formatter%put_var('latitude',this%lats,_RC)
    call this%formatter%put_var('station_id',this%station_id,_RC)

    _RETURN(_SUCCESS)
  end subroutine create_file_handle


  subroutine close_file_handle(this,rc)
    class(StationSampler), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status
    if (trim(this%ofile) /= '') then
       if (mapl_am_i_root()) then
          call this%formatter%close(_RC)
       end if
    end if
    _RETURN(_SUCCESS)
  end subroutine close_file_handle


  function compute_time_for_current(this,current_time,rc) result(rtimes)
    class(StationSampler), intent(inout) :: this
    type(ESMF_Time), intent(in) :: current_time
    integer, optional, intent(out) :: rc
    real(ESMF_KIND_R8), allocatable :: rtimes(:)
    integer :: i,status
    type(ESMF_TimeInterval) :: tint
    type(ESMF_Time) :: file_start_time
    character(len=ESMF_MAXSTR) :: tunit

    allocate(rtimes(1),_STAT)
    call this%get_file_start_time(file_start_time,tunit,_RC)
    tint = current_time-file_start_time
    select case(trim(tunit))
    case ('days')
       call ESMF_TimeIntervalGet(tint,d_r8=rtimes(1),_RC)
    case ('hours')
       call ESMF_TimeIntervalGet(tint,h_r8=rtimes(1),_RC)
    case ('minutes')
       call ESMF_TimeIntervalGet(tint,m_r8=rtimes(1),_RC)
    case default
       _FAIL('illegal value for tunit: '//trim(tunit))
    end select
    _RETURN(_SUCCESS)
  end function compute_time_for_current


  !-- a subroutine from MAPL_HistoryTrajectoryMod.F90
  !   TODO: consolidate with trajectory
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
          if (present(rc)) rc = -1
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
          if (TimeUnits(strlen:strlen) /= C_NULL_CHAR) then
             TimeUnits = trim(TimeUnits)//C_NULL_CHAR
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

  ! TODO: delete and use system utilities when available
  Subroutine count_substring (str, t, ncount)
    character (len=*), intent(in) :: str
    character (len=*), intent(in) :: t
    integer, intent(out) :: ncount
    integer :: i, j, k, lt
    ncount=0
    k=1
    lt = len(t) - 1
    do
       i=index(str(k:), t)
       if (i==0) exit
       ncount = ncount + 1
       k=k+i+lt
    end do
  end subroutine count_substring

end module StationSamplerMod

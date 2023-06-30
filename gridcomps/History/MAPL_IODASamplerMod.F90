#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module IODASamplerMod

  use ESMF
  use MAPL_ErrorHandlingMod
  use MAPL_KeywordEnforcerMod
  use MAPL_FileMetadataUtilsMod
  use LocStreamFactoryMod
  use pFIO
  use netcdf
  use MAPL_GriddedIOItemVectorMod
  use MAPL_GriddedIOItemMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_LocstreamRegridderMod
  use MAPL_plain_netCDF_Time
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  private

  public :: iodaSampler

  type :: iodaSampler
     private
     type(LocStreamFactory)         :: LSF
     type(ESMF_LocStream)           :: esmf_ls
     type(LocstreamRegridder)       :: regridder
     type(ESMF_Clock)               :: clock
     type(ESMF_Alarm), public       :: alarm

     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)     
     real(kind=REAL32), allocatable :: obs_time(:)
     real(kind=REAL64), allocatable :: X_full(:)
     real(kind=REAL64), allocatable :: Y_full(:)     
     real(kind=REAL64), allocatable :: T_full(:)     

     type(ESMF_FieldBundle)         :: bundle
     type(ESMF_FieldBundle)         :: output_bundle
     type(FileMetadata)             :: metadata
     type(NetCDF4_FileFormatter)    :: formatter
     type(VerticalData)             :: vdata
     type(TimeData)                 :: time_info
     type(GriddedIOitemVector)      :: items
     character(LEN=ESMF_MAXPATHLEN) :: ofile
     integer                        :: obs_written

     character(len=ESMF_MAXSTR)     :: nc_index
     character(len=ESMF_MAXSTR)     :: obs_file
     character(len=ESMF_MAXSTR)     :: nc_time
     character(len=ESMF_MAXSTR)     :: nc_latitude
     character(len=ESMF_MAXSTR)     :: nc_longitude
     character(len=ESMF_MAXSTR)     :: var_name_time
     character(len=ESMF_MAXSTR)     :: var_name_lat
     character(len=ESMF_MAXSTR)     :: var_name_lon
     character(len=ESMF_MAXSTR)     :: tunit
     integer :: epoch                         ! unit: second
     integer(ESMF_KIND_I8) :: epoch_index(2)  ! location index: js, je
     integer :: nlocation_full
     integer :: nlocation

   contains
!     procedure :: initialize
     procedure :: create_variable
     procedure :: create_grid     
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: append_file
!     procedure :: get_current_interval
!     procedure :: compute_times_for_interval
!     procedure :: create_output_bundle
!     procedure :: get_file_start_time
!     procedure :: get
!     procedure :: reset_times_to_current_day

     procedure :: add_metadata_route_handle
!     procedure :: create_LS_RH
!     procedure :: destroy_LS_RH
!     procedure :: regrid_accumulate     
     
  end type iodaSampler

  
  interface iodaSampler
     module procedure  iodaSampler_from_config
  end interface iodaSampler

contains
        

  function iodaSampler_from_config(clock,config,string,rc) result(sp)
    implicit none
    type(iodaSampler)          :: sp
    type(ESMF_Clock),  intent(in)    :: clock
    type(ESMF_Config), intent(inout) :: config
    character(len=*), intent(in)     :: string         
    integer, optional, intent(out)   :: rc
    integer :: status

    type(NetCDF4_FileFormatter) :: formatter
    type(FileMetadataUtils) :: metadata
    type(FileMetadata) :: basic_metadata
    integer :: num_times

    real(kind=REAL32), allocatable :: data_real32(:)
    integer :: ncid, grpid, ncid0
    integer :: dimid(10),  dimlen(10)
    integer :: len
    character(len=ESMF_MAXSTR) :: grp_name
    character(len=ESMF_MAXSTR) :: dim_name(10)
    character(len=ESMF_MAXSTR) :: var_name_lon
    character(len=ESMF_MAXSTR) :: var_name_lat
    character(len=ESMF_MAXSTR) :: var_name_time

    character(len=ESMF_MAXSTR) :: time_string
    integer :: time_integer
    type(ESMF_Time)            :: RingTime_epoch
    type(ESMF_Time)            :: startTime
    type(ESMF_Time)         :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_TimeInterval)  :: Frequency_epoch
    

    real(REAL64), allocatable :: wksp(:)
    integer, allocatable :: iwksp(:)
    type(FileMetadata) :: fmd_new

    integer :: yy, mm, dd, h, m, s, sec, second
    integer :: i, j
    
    type(ESMF_Time) :: time0
    integer (ESMF_KIND_I8) :: j0, j1, jt, jt1, jt2
    real(ESMF_KIND_R8) :: jx0, jx1
    real(ESMF_KIND_R8) :: x0, x1
    integer :: khi, klo, k, nstart, max_iter


    !__ clock and alarm
    !
    sp%clock = clock
    call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
    call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(string)//'Epoch:', default=0, _RC)
    _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
    sp%Epoch = time_integer
    call hms_2_s (time_integer, second, _RC)
    call ESMF_TimeIntervalSet(frequency_epoch, s=second, _RC)
    sp%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency_epoch, &
         RingTime=currTime, sticky=.false., _RC )

    !__ nc_info
    !    
    call ESMF_ConfigGetAttribute(config, value=sp%obs_file, default="", &
         label=trim(string) // 'obs_file:', _RC)
    call ESMF_ConfigGetAttribute(config, value=sp%nc_index, default="", &
         label=trim(string) // 'nc_Index:', _RC)
    call ESMF_ConfigGetAttribute(config, value=sp%nc_time, default="", &
         label=trim(string) // 'nc_Time:', _RC)
    call ESMF_ConfigGetAttribute(config, value=sp%nc_longitude, default="", &
         label=trim(string) // 'nc_Longitude:', _RC)
    call ESMF_ConfigGetAttribute(config, value=sp%nc_latitude, default="", &
         label=trim(string) // 'nc_Latitude:', _RC)

    !
    ! -- BUG:  get time unit from NC
    !    
    sp%tunit='seconds since 1970-01-01T00:00:00Z'


    !!    call create_grid (sp, _RC)
    call sp%create_grid(_RC)

    _RETURN(_SUCCESS)

  end function iodaSampler_from_config


  subroutine create_grid (this, rc) 
    implicit none
    class(iodaSampler)         :: this 
    integer, optional, intent(out)   :: rc
    integer :: status

    type(NetCDF4_FileFormatter) :: formatter
    type(FileMetadataUtils) :: metadata
    type(FileMetadata) :: basic_metadata
    integer :: num_times

    character(len=ESMF_MAXSTR) :: filename         
    character(len=5) :: obs_type
    character(len=ESMF_MAXSTR)         :: obs_file
    character(len=ESMF_MAXSTR)         :: nc_index
    character(len=ESMF_MAXSTR)         :: nc_time
    character(len=ESMF_MAXSTR)         :: nc_latitude
    character(len=ESMF_MAXSTR)         :: nc_longitude
    character(len=ESMF_MAXSTR)         :: tunit
    
    !!        type(FileMetadata) :: fmd_new
    real(kind=REAL32), allocatable :: data_real32(:)
    integer :: ncid, grpid, ncid0
    integer :: dimid(10),  dimlen(10)
    integer :: len
    character(len=ESMF_MAXSTR) :: grp_name
    character(len=ESMF_MAXSTR) :: dim_name(10)
    character(len=ESMF_MAXSTR) :: var_name_lon
    character(len=ESMF_MAXSTR) :: var_name_lat
    character(len=ESMF_MAXSTR) :: var_name_time

    character(len=ESMF_MAXSTR) :: time_string
    integer :: time_integer
    type(ESMF_Time)            :: RingTime_epoch
    type(ESMF_Time)            :: startTime

    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_TimeInterval)  :: Frequency_epoch


    real(REAL64), allocatable :: wksp(:)
    integer, allocatable :: iwksp(:)
    type(FileMetadata) :: fmd_new

    integer :: yy, mm, dd, h, m, s, sec, second
    integer :: i, j

    type(ESMF_Time) :: time0
    integer (ESMF_KIND_I8) :: j0, j1, jt, jt1, jt2
    real(ESMF_KIND_R8) :: jx0, jx1
    real(ESMF_KIND_R8) :: x0, x1
    integer :: khi, klo, k, nstart, max_iter


    !__  read nc + sort time
    !

    obs_file = this%obs_file
    nc_index = this%nc_index
    nc_time = this%nc_time
    nc_longitude = this%nc_longitude
    nc_latitude = this%nc_latitude
    tunit = this%tunit
    filename = obs_file
    dim_name(1) = trim(nc_index)         
    i=index(nc_longitude, '/')
    if( i > 0 ) then
       grp_name = nc_latitude(1:i-1)
    else
       grp_name = ''
       _FAIL('lat/lon name wo grp_name not implemented in iodaSampler from_config')
    endif
    var_name_lat = nc_latitude(i+1:)
    var_name_lon = nc_longitude(i+1:)
    var_name_time= nc_time(i+1:)

    this%var_name_lat =  var_name_lat 
    this%var_name_lon =  var_name_lon 
    this%var_name_time=  var_name_time
    
    print*, __FILE__, __LINE__
    print*, trim(obs_file)
    print*, trim(nc_latitude)
    print*, trim(nc_index)    
    print*, 'grp_name:', trim(grp_name)

    call formatter%open(filename, pFIO_READ, _RC)
    fmd_new=formatter%read(_RC)
    ncid0=formatter%ncid
    call check_nc_status( nf90_inq_ncid(ncid0, grp_name, ncid), _RC )
    grpid=ncid
    do i=1, 1
       call check_nc_status( nf90_inq_dimid(ncid, dim_name(i), dimid(i)), _RC )
       call check_nc_status( nf90_inquire_dimension(ncid, dimid(i), len=dimlen(i)), _RC )
    end do

    len=dimlen(1)
    this%nlocation_full = len
    allocate(this%X_full(len),_STAT)
    allocate(this%Y_full(len),_STAT)    
    allocate(this%T_full(len),_STAT)    

    call formatter%get_var(var_name_lon,  this%X_full, group_name=grp_name, count=[len], rc=status)
    call formatter%get_var(var_name_lat,  this%Y_full, group_name=grp_name, count=[len], rc=status)
    call formatter%get_var(var_name_time, this%T_full, group_name=grp_name, count=[len], rc=status)    
    !    call check_nc_status( nf90_get_att(ncid, dim_name(i), dimid(i)), _RC )    
    !    ./NetCDF4_FileFormatter.F90


    allocate( wksp(len) )
    allocate( iwksp(len) )    
    call sort3(len, this%T_full, this%X_full, this%Y_full, wksp, iwksp)
    write(6,*) 'af get_var:  data_real32'
    write(6,*)  this%T_full(1:len:5000)
    deallocate (wksp)
    deallocate (iwksp)

    nstart = 1 
    call ESMF_ClockGet(this%clock,currTime=time0,_RC)
    call time_esmf_2_nc_int (time0, this%tunit, j0, _RC)
    call hms_2_s (this%Epoch, sec, _RC)
    jx0= j0
    jx1= j0 + sec
    write(6,*) 'len_full:', len
    write(6,*) 'Epoch:', this%Epoch
    write(6,*) 'jx0, jx1', jx0, jx1

    call bisect( this%T_full, jx0, jt1, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(len, ESMF_KIND_I8), _RC )
    call bisect( this%T_full, jx1, jt2, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(len, ESMF_KIND_I8), _RC )

    if (jt1==jt2) then
       _FAIL('Epoch Time is too small, empty interval is generated, increase Epoch, STOP!')
    endif
    jt1 = jt1 + 1               ! (x1,x2]  design
    this%epoch_index(1)= jt1
    this%epoch_index(2)= jt2

    this%nlocation = jt2 - jt1 + 1
    allocate(this%lons(this%nlocation),_STAT)
    allocate(this%lats(this%nlocation),_STAT)    
    allocate(this%obs_time(this%nlocation),_STAT)

    k=0
    do j=this%epoch_index(1), this%epoch_index(2)
       k=k+1
       this%lons(k) = this%X_full(j)
       this%lats(k) = this%Y_full(j)
       this%obs_time(k) = this%T_full(j)       
    end do

    !__ 2. create LocStreamFactory, then esmf_ls including route_handle
    !
    this%LSF     = LocStreamFactory(this%lons, this%lats, _RC)
    this%esmf_ls = this%LSF%create_locstream(_RC)

    deallocate(this%X_full)
    deallocate(this%Y_full)
    deallocate(this%T_full)
    deallocate(this%lons)
    deallocate(this%lats)
    !
    ! leave time untouched
    !
    
    write(6,*) 'epoch_index(1:2)', this%epoch_index(1:2)


    _RETURN(_SUCCESS)

  end subroutine create_grid



  subroutine add_metadata_route_handle (this,bundle,vdata,rc)
    class(iodaSampler),  intent(inout)       :: this
    type(ESMF_FieldBundle), intent(in)          :: bundle
    type(VerticalData), optional, intent(inout) :: vdata
    integer, optional, intent(out)              :: rc

    type(variable)   :: v
    type(ESMF_Grid)  :: grid
    type(ESMF_Clock) :: clock
    type(ESMF_Field) :: field
    integer          :: fieldCount
    integer          :: fieldCount_max = 1000
    integer          :: field_rank
    integer          :: nloc_full
    integer          :: nloc    
    logical          :: is_present
    integer          :: ub(ESMF_MAXDIM)
    integer          :: lb(ESMF_MAXDIM)
    logical          :: do_vertical_regrid
    integer          :: status
    integer          :: i

    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character (len=ESMF_MAXSTR)    :: var_name, long_name, units, vdims
    character(len=ESMF_MAXSTR)     :: nc_index
    character(len=ESMF_MAXSTR)     :: obs_file
    character(len=ESMF_MAXSTR)     :: nc_time
    character(len=ESMF_MAXSTR)     :: nc_latitude
    character(len=ESMF_MAXSTR)     :: nc_longitude

    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item    
    

    !__ 1. metadata add_dimension,
    !     add_variable for latlon, dateTime
    !
    
    nc_index = this%nc_index

    this%bundle = bundle
    nloc_full = this%nlocation_full
    if (present(vdata)) then
       this%vdata = vdata
    else
       this%vdata = VerticalData(_RC)
    end if
    call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC) ! specify lev in fmd
    do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
    if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%get_interpolating_variable(this%bundle,_RC)
    endif
    
    call this%metadata%add_dimension(nc_index,nloc_full)

    v = Variable(type=pFIO_REAL32, dimensions=nc_index)
    call v%add_attribute('long_name','longitude')
    call v%add_attribute('unit','degree_east')
    call this%metadata%add_variable(this%name_var_lon,v)

    v = Variable(type=pFIO_REAL32, dimensions=nc_index)
    call v%add_attribute('long_name','latitude')
    call v%add_attribute('unit','degree_north')
    call this%metadata%add_variable(this%name_var_lat,v)

    v = Variable(type=pFIO_INT64, dimensions=nc_index)
    call v%add_attribute('unit', trim(this%tunit))
    call this%metadata%add_variable(this%name_var_time,v)
    
    iter = this%items%begin() 
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then
          call this%create_variable(item%xname,_RC)
       else if (item%itemType == ItemTypeVector) then
          call this%create_variable(item%xname,_RC)
          call this%create_variable(item%yname,_RC)
       end if
       call iter%next()
    enddo
    

    !__  locstream route handle
    !
    call ESMF_FieldBundleGet(bundle,grid=grid,_RC)
    this%regridder = LocStreamRegridder(grid,this%esmf_ls,_RC)
    _RETURN(_SUCCESS)

  end subroutine add_metadata_route_handle


  
  subroutine create_variable(this,vname,rc)
    class(iodaSampler), intent(inout) :: this
    character(len=*), intent(in) :: vname
    integer, optional, intent(out) :: rc

    integer :: status,field_rank
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
    type(variable) :: v
    logical :: is_present

    call ESMF_FieldBundleGet(this%bundle,vname,field=field,_RC)
    call ESMF_FieldGet(field,name=var_name,rank=field_rank,_RC)
    call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=is_present,_RC)
    if ( is_present ) then
       call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=long_name, _RC)
    else
       long_name = var_name
    endif
    call ESMF_AttributeGet(field,name="UNITS",isPresent=is_present,_RC)
    if ( is_present ) then
       call ESMF_AttributeGet  (FIELD, NAME="UNITS",VALUE=units, _RC)
    else
       units = 'unknown'
    endif
    if (field_rank==2) then
       vdims = this%nc_index
    else if (field_rank==3) then
       vdims = trim(this%nc_index)//',lev'
    end if
    v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
    call v%add_attribute('units',trim(units))
    call v%add_attribute('long_name',trim(long_name))
    call v%add_attribute('missing_value',MAPL_UNDEF)
    call v%add_attribute('_FillValue',MAPL_UNDEF)
    call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
    call this%metadata%add_variable(trim(var_name),v,_RC)

  end subroutine create_variable



  subroutine create_file_handle(this,filename,rc)
    class(iodaSampler), intent(inout) :: this
    character(len=*), intent(inout) :: filename  ! for ouput nc
    integer, optional, intent(out) :: rc
    type(variable) :: v
    integer :: status

    this%ofile = trim(filename)
    v = this%time_info%define_time_variable(_RC)
    call this%metadata%modify_variable('time',v,_RC)
    this%obs_written = 0

    if (.not. mapl_am_I_root()) then
       _RETURN(_SUCCESS)
    end if
    call this%formatter%create(trim(filename),_RC)
    call this%formatter%write(this%metadata,_RC)
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


         
end module IODASamplerMod

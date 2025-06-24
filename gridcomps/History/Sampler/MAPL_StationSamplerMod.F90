#include "MAPL_Generic.h"
#include "MAPL_ErrLog.h"
module StationSamplerMod
  use ESMF
  use MAPL_ErrorHandlingMod
  use LocStreamFactoryMod
  use pFIO
  use MAPL_GriddedIOItemMod
  use MAPL_GriddedIOItemVectorMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_LocstreamRegridderMod
  use MAPL_GenericMod, only : MAPL_MetaComp, MAPL_TimerOn, MAPL_TimerOff
  use MPI, only  :  MPI_INTEGER, MPI_REAL, MPI_REAL8
  use, intrinsic :: iso_fortran_env, only: INT64
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use, intrinsic :: iso_c_binding,   only: C_NULL_CHAR
  implicit none
  private

  public :: StationSampler
  type :: StationSampler
     private
     type(LocStreamFactory)   :: LSF
     type(ESMF_LocStream)     :: LS_rt
     type(ESMF_LocStream)     :: LS_chunk
     type(ESMF_LocStream)     :: LS_ds
     type(LocstreamRegridder) :: regridder
     type(ESMF_RouteHandle)   :: RH
     type(GriddedIOitemVector) :: items
     logical :: do_vertical_regrid
     logical :: level_by_level
     type(MAPL_MetaComp), pointer   :: GENSTATE

     integer                  :: nstation
     integer, allocatable :: station_id(:)
     character(len=ESMF_MAXSTR), allocatable :: station_name(:)
     character(len=ESMF_MAXSTR), allocatable :: station_fullname(:)
     character(len=ESMF_MAXSTR) ::  index_name_x
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL64), allocatable :: elevs(:)
     type(ESMF_FieldBundle)         :: bundle
     type(FileMetadata)             :: metadata
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
     procedure                      :: create_variable => create_metadata_variable
  end type StationSampler

  interface StationSampler
     module procedure new_StationSampler_readfile
  end interface StationSampler

contains

  function new_StationSampler_readfile (bundle, filename, nskip_line, GENSTATE, rc) result(sampler)
    use pflogger, only             :  Logger, logging
    implicit none
    type(StationSampler)           :: sampler
    type(ESMF_FieldBundle), intent(in) :: bundle
    character(len=*), intent(in)   :: filename
    integer, optional, intent(in)  :: nskip_line
    type(MAPL_MetaComp), pointer, intent(in), optional  :: GENSTATE
    integer, optional, intent(out) :: rc

    type(ESMF_VM) :: vm
    integer :: mypet, petcount, mpic
    type(ESMF_grid) :: grid
    integer, allocatable :: sendcount(:), displs(:)
    integer :: recvcount
    integer :: is, ie, ierr
    integer :: M, N, ip
    integer :: arr(1)
    integer :: nx, nx2, nx_sum

    real(REAL64), allocatable :: lons_chunk(:)
    real(REAL64), allocatable :: lats_chunk(:)

    integer :: unit, ios, nstation, status
    integer :: i, j, k, ncount
    logical :: con1, con2
    character (len=1)     :: CH1
    character (len=5)     :: seq
    character (len=100)   :: line, line2
    integer               :: nskip
    type(Logger), pointer :: lgr

    !__ 1. read from station_id_file: static
    !      plain text format:
    !      ["name,lat,lon,elev"] or ["id,name,lat,lon,elev"]
    !      ["name_short lat lon elev name_full"]
    !

    lgr => logging%get_logger('HISTORY.sampler')
    if ( MAPL_AM_I_ROOT() ) then
       open(newunit=unit, file=trim(filename), form='formatted', &
            access='sequential', status='old', _iostat)
       ios=0
       nstation=0
       nskip=0
       if (present(nskip_line)) then
          nskip=nskip_line
       end if
       if (nskip>0) then
          do i=1, nskip
             read(unit, *)
          end do
       end if
       read(unit, '(a100)', IOSTAT=ios) line
       call count_substring(line, ',', ncount, _rc)
       con1= (ncount>=2 .AND. ncount<=4).OR.(ncount==0)
       _assert(con1, 'string sequence in Aeronet file not supported')
       if (ncount==0) then
          seq='AFFFA'
       elseif (ncount==2) then
          seq='AFF'
       elseif (ncount==3) then
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
                _assert(.false., 'string sequence in Aeronet file not supported')
             end if
          end if
       end if

       rewind(unit)
       if (nskip>0) then
          do i=1, nskip
             read(unit, *)
          end do
       end if
       ios=0
       do while (ios==0)
          read(unit, '(a100)', IOSTAT=ios) line
          if (ios==0) nstation=nstation+1
       end do
       sampler%nstation=nstation
       allocate(sampler%station_id(nstation), _stat)
       allocate(sampler%station_name(nstation), _stat)
       allocate(sampler%station_fullname(nstation), _stat)
       allocate(sampler%lons(nstation), _stat)
       allocate(sampler%lats(nstation), _stat)
       allocate(sampler%elevs(nstation), _stat)

       rewind(unit)
       if (nskip>0) then
          do i=1, nskip
             read(unit, *)
          end do
       end if
       do i=1, nstation
          if(seq=='IAFFF') then
             read(unit, *) &
                  sampler%station_id(i), &
                  sampler%station_name(i), &
                  sampler%lons(i), &
                  sampler%lats(i)
          elseif(seq=='AIFFF') then
             read(unit, *) &
                  sampler%station_name(i), &
                  sampler%station_id(i), &
                  sampler%lons(i), &
                  sampler%lats(i)
          elseif(trim(seq)=='AFF' .OR. trim(seq)=='AFFF') then
             !!write(6,*) 'i=', i
             line=''
             read(unit, '(a100)')  line
             !!write(6,*) 'line=', trim(line)
             call CSV_read_line_with_CH_I_R(line, &
                  sampler%station_name(i), &
                  sampler%lons(i), &
                  sampler%lats(i), _rc)
             sampler%station_id(i)=i
          elseif(trim(seq)=='AFFFA') then
          ! NOAA GHCNd
          ! Ex: 'CHM00054511  39.9330  116.2830   55.0    BEIJING   GSN     54511'
             read(unit, *) &
                  sampler%station_name(i), &
                  sampler%lats(i), &
                  sampler%lons(i)
             sampler%station_id(i)=i
             backspace(unit)
             read(unit, '(a100)', IOSTAT=ios) line
             j=index(line, '.', BACK=.true.)
             line2=line(j+1:)
             k=len(line2)
             line=''
             do j=1, k
                CH1=line2(j:j)
                con1= (CH1>='a'.AND.CH1<='z').OR.(CH1>='A'.AND.CH1<='Z')
                if (con1) exit
             enddo
             read(line2(j:k), '(a100)') line
             line2=trim(line)
             k=len(line2)
             line=''
             do j=1, k
                CH1=line2(j:j)
                con1= (CH1>='0' .AND. CH1<='9')
                if (con1) exit
             enddo
             if (j>k) j=k
             sampler%station_fullname(i) = trim(line2(1:j-1))
          end if
       end do
       close(unit)

       write(6,*)  'nstation=', nstation
       write(6,*)  'sampler%station_name(1:2) : ', &
            trim(sampler%station_name(1)), trim(sampler%station_name(2))
       write(6,*)  'sampler%lons(1:2) : ',&
            sampler%lons(1:2)
       write(6,*)  'sampler%lats(1:2) : ',&
            sampler%lats(1:2)
    else
       nstation=0
       sampler%nstation = 0
       allocate(sampler%station_id(nstation), _stat)
       allocate(sampler%station_name(nstation), _stat)
       allocate(sampler%station_fullname(nstation), _stat)
       allocate(sampler%lons(nstation), _stat)
       allocate(sampler%lats(nstation), _stat)
       allocate(sampler%elevs(nstation), _stat)
    end if
    sampler%index_name_x = 'station_index'
    if (present(GENSTATE)) sampler%GENSTATE => GENSTATE


    !__ 2. create LocStreamFactory, then LS_rt including route_handle
    !
    !      grid_A:  LS_rt    : on root
    !      grid_B:  LS_chunk : uniform on cores
    !      grid_C:  LS_ds    : bg=CS
    !
    call ESMF_VMGetCurrent(vm,_rc)
    call ESMF_VMGet(vm, mpiCommunicator=mpic, petCount=petCount, localPet=mypet, _rc)
    call MAPL_CommsBcast(vm, DATA=sampler%nstation, N=1, ROOT=MAPL_Root, _rc)

    nx_sum = sampler%nstation
    ip = mypet    ! 0 to M-1
    N = nx_sum
    M = petCount
    recvcount = int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
         int(ip  , INT64) * int(N, INT64) / int(M, INT64)
    call lgr%debug('%a %i12 %i12', 'ip, recvcount', ip, recvcount)

    allocate ( sendcount (petCount) )
    allocate ( displs    (petCount) )
    do ip=0, M-1
       sendcount(ip+1) = int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
            int(ip  , INT64) * int(N, INT64) / int(M, INT64)
    end do
    displs(1)=0
    do i = 2, petCount
       displs(i) = displs(i-1) + sendcount(i-1)
    end do

    allocate ( lons_chunk (recvcount) )
    allocate ( lats_chunk (recvcount) )

    arr(1) = recvcount
    call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx2, &
         count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    _assert( nx2 == nx_sum, 'Erorr in recvcount' )

    call MPI_Scatterv( sampler%lons, sendcount, &
         displs, MPI_REAL8,  lons_chunk, &
         recvcount, MPI_REAL8, 0, mpic, ierr)
    _verify(ierr)

    call MPI_Scatterv( sampler%lats, sendcount, &
         displs, MPI_REAL8,  lats_chunk, &
         recvcount, MPI_REAL8, 0, mpic, ierr)
    _verify(ierr)

    ! -- root
    sampler%LSF   = LocStreamFactory(sampler%lons, sampler%lats, _rc)
    sampler%LS_rt = sampler%LSF%create_locstream(_rc)

    ! -- chunk
    sampler%LSF = LocStreamFactory(lons_chunk,lats_chunk,_rc)
    sampler%LS_chunk = sampler%LSF%create_locstream_on_proc(_rc)

    ! -- distributed
    call ESMF_FieldBundleGet(bundle,grid=grid,_rc)
    sampler%LS_ds = sampler%LSF%create_locstream_on_proc(grid=grid,_rc)

    ! init ofile
    sampler%ofile=''
    sampler%obs_written=0
    sampler%level_by_level = .false.

    _return(_success)
  end function new_StationSampler_readfile


  subroutine add_metadata_route_handle (this,items,bundle,timeInfo,vdata,rc)
    class(StationSampler),  intent(inout)       :: this
    type(GriddedIOitemVector), optional, intent(inout) :: items
    type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
    type(TimeData), optional, intent(inout)           :: timeInfo
    type(VerticalData), optional, intent(inout) :: vdata
    integer, optional, intent(out)              :: rc

    type(variable)   :: v
    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    type(ESMF_Grid)  :: grid
    type(ESMF_Field) :: field
    integer          :: fieldCount
    integer          :: field_rank
    integer          :: nstation
    logical          :: is_present
    integer          :: ub(ESMF_MAXDIM)
    integer          :: lb(ESMF_MAXDIM)
    logical          :: do_vertical_regrid
    integer          :: status
    integer          :: i, lm

    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: var_name, long_name, units, vdims

    type(ESMF_Field) :: src_field, chunk_field
    real(REAL32), pointer :: pt1(:), pt2(:)


    !__ 1. filemetadata:
    !      add_dimension, add_variable for latlon, station
    !
    if(present(bundle))   this%bundle=bundle
    if(present(items))    this%items=items
    if(present(timeInfo)) this%time_info=timeInfo
    if (present(vdata)) then
       this%vdata = vdata
    else
       this%vdata = VerticalData(_rc)
    end if
    nstation = this%nstation

    call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_rc) ! specify lev in fmd
    do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
    if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%get_interpolating_variable(this%bundle,_rc)
    endif

    call timeInfo%add_time_to_metadata(this%metadata,_rc) ! specify time in fmd
    this%time_info = timeInfo

    call this%metadata%add_dimension('station_index',nstation)

    v = Variable(type=pFIO_REAL32, dimensions='station_index')
    call v%add_attribute('long_name','longitude')
    call v%add_attribute('unit','degree_east')
    call this%metadata%add_variable('longitude',v)

    v = Variable(type=pFIO_REAL32, dimensions='station_index')
    call v%add_attribute('long_name','latitude')
    call v%add_attribute('unit','degree_north')
    call this%metadata%add_variable('latitude',v)

    v = Variable(type=pFIO_INT32, dimensions='station_index')
    call this%metadata%add_variable('station_id',v)
    v = Variable(type=pFIO_STRING, dimensions='station_index')
    call v%add_attribute('long_name','station name')
    call this%metadata%add_variable('station_name',v)


    !__ 2. filemetadata:
    !      create varible with names in item%xname; see create_metadata_variable
    !
    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then
          call this%create_variable(item%xname,_rc)
       else if (item%itemType == ItemTypeVector) then
          call this%create_variable(item%xname,_rc)
          call this%create_variable(item%yname,_rc)
       end if
       call iter%next()
    enddo


    !__ 3. route handle  CS --> LS_ds
    !
    call ESMF_FieldBundleGet(bundle,grid=grid,_rc)
    this%regridder = LocStreamRegridder(grid,this%LS_ds,_rc)

    !__ 4. route handle  LS_ds --> LS_chunk
    !
    src_field = ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_rc)
    chunk_field = ESMF_FieldCreate(this%LS_chunk,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_rc)
    call ESMF_FieldGet( src_field, localDE=0, farrayPtr=pt1, _rc )
    call ESMF_FieldGet( chunk_field, localDE=0, farrayPtr=pt2, _rc )
    pt1=0.0
    pt2=0.0
    call ESMF_FieldRedistStore(src_field,chunk_field,this%RH,_rc)
    call ESMF_FieldDestroy(src_field,noGarbage=.true.,_rc)
    call ESMF_FieldDestroy(chunk_field,noGarbage=.true.,_rc)

    _return(_success)
  end subroutine add_metadata_route_handle


  subroutine  create_metadata_variable(this,vname,rc)
    class(StationSampler), intent(inout) :: this
    character(len=*), intent(in)         :: vname
    integer, optional, intent(out)       :: rc

    type(ESMF_Field) :: field
    type(variable) :: v
    logical :: is_present
    integer :: field_rank, status
    character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
    integer :: rank,lb(1),ub(1)
    integer :: k, ig
    integer, allocatable :: chunksizes(:)

    call ESMF_FieldBundleGet(this%bundle,vname,field=field,_rc)
    call ESMF_FieldGet(field,name=var_name,rank=field_rank,_rc)
    call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=is_present,_rc)
    long_name = var_name
    if ( is_present ) then
       call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=long_name, _rc)
    endif
    call ESMF_AttributeGet(field,name="UNITS",isPresent=is_present,_rc)
    units = 'unknown'
    if ( is_present ) then
       call ESMF_AttributeGet  (FIELD, NAME="UNITS",VALUE=units, _rc)
    endif

    vdims = "station_index,time"
    select case (field_rank)
    case(2)
       chunksizes = [this%nstation,1]
    case(3)
       vdims = "lev,"//trim(vdims)
       call ESMF_FieldGet(field,ungriddedLBound=lb,ungriddedUBound=ub,_rc)
       chunksizes = [ub(1)-lb(1)+1,1,1]
    case default
       _fail('unsupported rank')
    end select
    v = variable(type=PFIO_REAL32,dimensions=trim(vdims))

    call v%add_attribute('units',trim(units))
    call v%add_attribute('long_name',trim(long_name))
    call v%add_attribute('missing_value',MAPL_UNDEF)
    call v%add_attribute('_FillValue',MAPL_UNDEF)
    call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
    call this%metadata%add_variable(trim(var_name),v,_rc)

    _return(_success)
  end subroutine create_metadata_variable



  subroutine append_file(this,current_time,rc)
    class(StationSampler), intent(inout) :: this
    type(ESMF_Time), intent(in) :: current_time
    integer, optional, intent(out) :: rc
    !
    integer :: status
    integer :: fieldCount
    integer :: ub(1), lb(1)
    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    type(ESMF_Grid)  :: grid
    type(ESMF_Field) :: src_field  !  ,dst_field
    type(ESMF_Field) :: new_src_field,new_dst_field
    real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:), qin_3d(:,:,:)   ! source
    real(kind=REAL32), pointer :: p_dst_3d(:,:)                    ! destination
    real(kind=REAL32), pointer :: p_ds_3d(:,:),p_ds_2d(:)          ! distributed LS
    real(kind=REAL32), pointer :: p_chunk_3d(:,:),p_chunk_2d(:)    ! chunk LS
    real(kind=REAL32), pointer :: p_rt_3d(:,:),p_rt_2d(:)          ! root LS
    real(kind=REAL32), pointer :: p_rt_3d_aux(:,:)
    real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
    real(kind=REAL32), allocatable :: p_dst_t(:,:)

    real(kind=REAL32), allocatable :: arr(:,:)
    character(len=ESMF_MAXSTR), allocatable :: fieldNameList(:)
    character(len=ESMF_MAXSTR) :: xname
    real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)

    integer :: rank
    integer :: i, j, k, nz, lm

    type(ESMF_VM) :: vm
    integer :: mypet, petcount, mpic, iroot
    integer :: n0, nx, nx2
    integer :: na, nb, nx_sum, nsend, nsend_v

    ! intermediate fields as placeholder
    type(ESMF_Field)               :: field_ds_2d
    type(ESMF_Field)               :: field_chunk_2d
    type(ESMF_Field)               :: field_chunk_3d

    integer :: sec
    integer, allocatable :: ix(:) !  counter for each obs(k)%nobs_epoch
    logical :: EX ! file
    logical :: zero_obs
    integer, allocatable :: recvcount(:), sendcount(:), displs(:)
    integer, allocatable :: recvcount_v(:), displs_v(:)
    integer :: is, ie, ierr
    integer :: M, N, ip

    this%obs_written=this%obs_written+1

    !__ 1. put_var: time variable
    !
    rtimes = this%compute_time_for_current(current_time,_rc) ! rtimes: seconds since opening file
    if (mapl_am_i_root()) then
       call this%formatter%put_var('time',rtimes(1:1),&
            start=[this%obs_written],count=[1],_rc)
    end if


    !__ 2. regrid + put_var:
    !      ungridded_dim from src to dst [regrid]
    !
    !      caution about zero-sized array for MPI
    !      redist
    !
    nx_sum = this%nstation
    lm = this%vdata%lm
    call ESMF_VMGetCurrent(vm,_rc)
    call ESMF_VMGet(vm, mpiCommunicator=mpic, petCount=petCount, localPet=mypet, _rc)

    iroot = 0
    ip = mypet
    N = nx_sum
    M = petCount
    nsend = int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
         int(ip  , INT64) * int(N, INT64) / int(M, INT64)
    allocate ( recvcount (petCount) )
    allocate ( displs    (petCount) )
    do ip=0, M-1
       recvcount(ip+1) =  int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
            int(ip  , INT64) * int(N, INT64) / int(M, INT64)
    end do
    displs(1)=0
    do i = 2, petCount
       displs(i) = displs(i-1) + recvcount(i-1)
    end do

    nsend_v = nsend * lm      ! vertical
    allocate (recvcount_v, source = recvcount * lm )
    allocate (displs_v, source = displs * lm )

    if (mapl_am_i_root()) then
       allocate ( p_rt_2d(nx_sum) )
    else
       allocate ( p_rt_2d(1) )
    end if

    ! p_rt_3d (lm, nx)
    if (mapl_am_i_root()) then
       allocate ( p_rt_3d(lm, nx_sum) )
       allocate ( p_rt_3d_aux(nx_sum, lm) )
    else
       allocate ( p_rt_3d(lm, 1) )
       allocate ( p_rt_3d_aux(1,lm) )
    end if


    !__  Aux. field
    !
    call MAPL_TimerOn(this%GENSTATE,"FieldCreate")

    call ESMF_FieldBundleGet(this%bundle,grid=grid,_rc)
    field_ds_2d    = ESMF_FieldCreate (this%LS_ds, name='field_2d_ds', typekind=ESMF_TYPEKIND_R4, _rc)
    field_chunk_2d = ESMF_FieldCreate (this%LS_chunk, name='field_2d_chunk', typekind=ESMF_TYPEKIND_R4, _rc)
    new_src_field  = ESMF_FieldCreate (grid, name='new_src_field', typekind=ESMF_TYPEKIND_R4, &
         gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[lm],_rc)
    new_dst_field  = ESMF_FieldCreate (this%LS_ds, name='new_dst_field', typekind=ESMF_TYPEKIND_R4, &
         gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],_rc)
    field_chunk_3d = ESMF_FieldCreate (this%LS_chunk, name='field_3d_chunk', typekind=ESMF_TYPEKIND_R4, &
         gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],_rc)

    call ESMF_FieldGet(field_ds_2d,   localDE=0, farrayptr=p_ds_2d,    _rc)
    call ESMF_FieldGet(field_chunk_2d,localDE=0, farrayPtr=p_chunk_2d, _rc)
    call ESMF_FieldGet(new_src_field, localDE=0, farrayPtr=p_src_3d,   _rc)
    call ESMF_FieldGet(new_dst_field, localDE=0, farrayPtr=p_dst_3d,   _rc)
    call ESMF_FieldGet(field_chunk_3d,localDE=0, farrayPtr=p_chunk_3d, _rc)

    call MAPL_TimerOff(this%GENSTATE,"FieldCreate")

    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then
          !! if (mapl_am_i_root()) write(6,*) 'item%xname=', trim(item%xname)
          call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_rc)
          call ESMF_FieldGet(src_field,rank=rank,_rc)
          select case (rank)
          case(2)
             call ESMF_FieldGet(src_field,localDE=0,farrayptr=p_src_2d,_rc)
             call ESMF_FieldRegrid (src_field, field_ds_2d, this%regridder%route_handle, _rc)
             call ESMF_FieldRedist (field_ds_2d, field_chunk_2d, this%RH, _rc )
             call MPI_gatherv ( p_chunk_2d, nsend, MPI_REAL, &
                  p_rt_2d, recvcount, displs, MPI_REAL,&
                  iroot, mpic, ierr )
             _verify(ierr)

             call MAPL_TimerOn(this%GENSTATE,"put2D")
             if (mapl_am_i_root()) then
                call this%formatter%put_var(trim(item%xname),p_rt_2d,&
                     start=[1,this%obs_written],count=[this%nstation,1],_rc)
             end if
             call MAPL_TimerOff(this%GENSTATE,"put2D")

          case(3)
             ! -- CS-> LS_ds; ds->chunk; gather
             !
             call ESMF_FieldGet(src_field,localDE=0,farrayptr=qin_3d,_rc)

             call MAPL_TimerOn(this%GENSTATE,"reshape")
             p_src_3d = reshape(qin_3d,shape(p_src_3d),order=[2,3,1])
             call MAPL_TimerOff(this%GENSTATE,"reshape")

             call MAPL_TimerOn(this%GENSTATE,"3d_regrid")
             call ESMF_FieldRegrid (new_src_field, new_dst_field, this%regridder%route_handle, _rc)
             call MAPL_TimerOff(this%GENSTATE,"3d_regrid")

             call MPI_Barrier(mpic,ierr)
             _verify(ierr)
             call MAPL_TimerOn(this%GENSTATE,"FieldRedist")
             call ESMF_FieldRedist (new_dst_field, field_chunk_3d, this%RH, _rc)
             call MPI_Barrier(mpic,ierr)
             _verify(ierr)
             call MAPL_TimerOff(this%GENSTATE,"FieldRedist")


             call MAPL_TimerOn(this%GENSTATE,"gatherv")
             if (this%level_by_level) then
                ! p_chunk_3d (lm, nx)
                allocate (p_dst_t, source = reshape(p_chunk_3d, [size(p_chunk_3d,2),size(p_chunk_3d,1)], order=[2,1]))
                do k = 1, lm
                   call MPI_gatherv ( p_dst_t(1,k), nsend, MPI_REAL, &
                        p_rt_3d_aux(1,k), recvcount, displs, MPI_REAL,&
                        iroot, mpic, ierr )
                   _verify(ierr)
                end do
                deallocate(p_dst_t)
                p_rt_3d = reshape(p_rt_3d_aux, shape(p_rt_3d), order=[2,1])
             else
                call MPI_gatherv ( p_chunk_3d, nsend_v, MPI_REAL, &
                     p_rt_3d, recvcount_v, displs_v, MPI_REAL,&
                     iroot, mpic, ierr )
                _verify(ierr)
             end if
             call MAPL_TimerOff(this%GENSTATE,"gatherv")


             call MAPL_TimerOn(this%GENSTATE,"put3D")
             if (mapl_am_i_root()) then
                nz=size(p_rt_3d,1); nx=size(p_rt_3d,2)
                call this%formatter%put_var(trim(item%xname),p_rt_3d,&
                     start=[1,1,this%obs_written],count=[nz,nx,1],_rc)
                !note:     lev,station,time
             end if
             call MAPL_TimerOff(this%GENSTATE,"put3D")
          case default
             _fail('grid2LS regridder: rank > 3 not implemented')
          end select
       else
          _fail ('ItemType vector not supported')
       endif

       call iter%next()
    end do


    call MAPL_TimerOn(this%GENSTATE,"FieldDestroy")
    call ESMF_FieldDestroy(field_ds_2d,    noGarbage=.true., _rc)
    call ESMF_FieldDestroy(field_chunk_2d, noGarbage=.true., _rc)
    call ESMF_FieldDestroy(field_chunk_3d, noGarbage=.true., _rc)
    call ESMF_FieldDestroy(new_dst_field,  noGarbage=.true., _rc)
    call ESMF_FieldDestroy(new_src_field,  noGarbage=.true., _rc)
    call MAPL_TimerOff(this%GENSTATE,"FieldDestroy")

    _return(_success)
  end subroutine append_file


  subroutine create_file_handle(this,filename,rc)
    class(StationSampler), intent(inout) :: this
    character(len=*), intent(inout) :: filename  ! for ouput nc
    integer, optional, intent(out) :: rc
    type(variable) :: v
    integer :: status, j

    this%ofile = trim(filename)
    v = this%time_info%define_time_variable(_rc)
    call this%metadata%modify_variable('time',v,_rc)
    this%obs_written = 0

    if (.not. mapl_am_I_root()) then
       _return(_success)
    end if
    call this%formatter%create(trim(filename),_rc)
    call this%formatter%write(this%metadata,_rc)
    call this%formatter%put_var('longitude',this%lons,_rc)
    call this%formatter%put_var('latitude',this%lats,_rc)
    call this%formatter%put_var('station_id',this%station_id,_rc)
    call this%formatter%put_var('station_name',this%station_name,_rc)

    _return(_success)
  end subroutine create_file_handle


  subroutine close_file_handle(this,rc)
    class(StationSampler), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status
    if (trim(this%ofile) /= '') then
       if (mapl_am_i_root()) then
          call this%formatter%close(_rc)
       end if
    end if
    _return(_success)
  end subroutine close_file_handle


  function compute_time_for_current(this,current_time,rc) result(rtimes)
    class(StationSampler), intent(inout) :: this
    type(ESMF_Time), intent(in) :: current_time
    integer, optional, intent(out) :: rc
    real(ESMF_KIND_R8), allocatable :: rtimes(:)
    integer :: status
    type(ESMF_TimeInterval) :: tint
    type(ESMF_Time) :: file_start_time
    character(len=ESMF_MAXSTR) :: tunit

    allocate(rtimes(1),_stat)
    call this%get_file_start_time(file_start_time,tunit,_rc)
    tint = current_time-file_start_time
    select case(trim(tunit))
    case ('days')
       call ESMF_TimeIntervalGet(tint,d_r8=rtimes(1),_rc)
    case ('hours')
       call ESMF_TimeIntervalGet(tint,h_r8=rtimes(1),_rc)
    case ('minutes')
       call ESMF_TimeIntervalGet(tint,m_r8=rtimes(1),_rc)
    case default
       _fail('illegal value for tunit: '//trim(tunit))
    end select
    _return(_success)
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

    var => this%metadata%get_variable('time',_rc)
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
       _fail("Time unit must be character")
    end select
    call ESMF_TimeSet(start_time,yy=year,mm=month,dd=day,h=hour,m=min,s=sec,_rc)
    _return(_success)
  end subroutine get_file_start_time


  ! TODO: delete and use system utilities when available
  Subroutine count_substring (str, t, ncount, rc)
    character (len=*), intent(in) :: str
    character (len=*), intent(in) :: t
    integer, intent(out) :: ncount
    integer, optional, intent(out) :: rc
    integer :: i, k, lt
    ncount=0
    k=1
    lt = len(t) - 1
    do
       i=index(str(k:), t)
       if (i==0) exit
       ncount = ncount + 1
       k=k+i+lt
    end do
    _return(_success)
  end subroutine count_substring


  subroutine CSV_read_line_with_CH_I_R(line, name, lon, lat, rc)
    character (len=*), intent(in) :: line
    character (len=*), intent(out) :: name
    real(kind=REAL64), intent(out) :: lon, lat
    integer, optional, intent(out) :: rc
    integer :: n
    integer :: i, j, k
    integer :: ios

    i=index(line, ',')
    j=index(line(i+1:), ',')
    _assert (i>0, 'not CSV format')
    _assert (j>0, 'CSV format: find only 1 comma, should be > 1')
    j=i+j

    read(line(1:i-1), '(a100)', iostat=ios)  name
    _assert (ios==0, 'read error')
    k=index(line(i+1:j-1), '.')
    if (k > 0) then
       read(line(i+1:j-1), *, iostat=ios) lon
    else
       read(line(i+1:j-1), *, iostat=ios) i
       lon = i
    endif
    _assert (ios==0, 'read error')

    k=index(line(j+1:), '.')
    if (k > 0) then
       read(line(j+1:), *, iostat=ios) lat
    else
       read(line(j+1:), *, iostat=ios) i
       lat = i
    endif
    _assert (ios==0, 'read error')

    !!write(6,*) trim(name), lon, lat
    _return(_success)

  end subroutine CSV_read_line_with_CH_I_R

end module StationSamplerMod

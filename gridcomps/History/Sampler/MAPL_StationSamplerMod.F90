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
     integer                  :: nstation
     integer, allocatable :: station_id(:)
     character(len=ESMF_MAXSTR), allocatable :: station_name(:)
     character(len=ESMF_MAXSTR), allocatable :: station_fullname(:)
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

  function new_StationSampler_readfile (bundle, filename, nskip_line, rc) result(sampler)
    use pflogger, only             :  Logger, logging
    implicit none
    type(StationSampler)           :: sampler
    type(ESMF_FieldBundle), intent(in) :: bundle
    character(len=*), intent(in)   :: filename
    integer, optional, intent(in)  :: nskip_line
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

    if ( MAPL_AM_I_ROOT() ) then       
       open(newunit=unit, file=trim(filename), form='formatted', &
            access='sequential', status='old', _IOSTAT)
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
       call count_substring(line, ',', ncount, _RC)
       con1= (ncount>=2 .AND. ncount<=4).OR.(ncount==0)
       _ASSERT(con1, 'string sequence in Aeronet file not supported')
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
                _ASSERT(.false., 'string sequence in Aeronet file not supported')
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
       allocate(sampler%station_id(nstation), _STAT)
       allocate(sampler%station_name(nstation), _STAT)
       allocate(sampler%station_fullname(nstation), _STAT)
       allocate(sampler%lons(nstation), _STAT)
       allocate(sampler%lats(nstation), _STAT)
       allocate(sampler%elevs(nstation), _STAT)

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
                  sampler%lats(i), _RC)
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
       lgr => logging%get_logger('HISTORY.sampler')
       call lgr%debug('%a %i8',   'nstation=', nstation)
       call lgr%debug('%a %a %a', 'sampler%station_name(1:2) : ', &
            trim(sampler%station_name(1)), trim(sampler%station_name(2)))
       call lgr%debug('%a %f8.2 %f8.2', 'sampler%lons(1:2) : ',&
            sampler%lons(1),sampler%lons(2))
       call lgr%debug('%a %f8.2 %f8.2', 'sampler%lats(1:2) : ',&
            sampler%lats(1),sampler%lats(2))
    else
       nstation=0
       allocate(sampler%station_id(nstation), _STAT)
       allocate(sampler%station_name(nstation), _STAT)
       allocate(sampler%station_fullname(nstation), _STAT)
       allocate(sampler%lons(nstation), _STAT)
       allocate(sampler%lats(nstation), _STAT)
       allocate(sampler%elevs(nstation), _STAT)
    end if


    !__ 2. create LocStreamFactory, then LS_rt including route_handle
    !
    !      grid_A:  LS_rt    : on root
    !      grid_B:  LS_chunk : uniform on cores
    !      grid_C:  LS_ds    : bg=CS
    !
    call ESMF_VMGetCurrent(vm,_RC)
    call ESMF_VMGet(vm, mpiCommunicator=mpic, petCount=petCount, localPet=mypet, _RC)
    call MAPL_CommsBcast(vm, DATA=sampler%nstation, N=1, ROOT=MAPL_Root, _RC)
        
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
    _ASSERT( nx2 == nx_sum, 'Erorr in recvcount' )

    call MPI_Scatterv( sampler%lons, sendcount, &
         displs, MPI_REAL8,  lons_chunk, &
         recvcount, MPI_REAL8, 0, mpic, ierr)

    call MPI_Scatterv( sampler%lats, sendcount, &
         displs, MPI_REAL8,  lats_chunk, &
         recvcount, MPI_REAL8, 0, mpic, ierr)

    ! -- root
    sampler%LSF   = LocStreamFactory(sampler%lons, sampler%lats, _RC)
    sampler%LS_rt = sampler%LSF%create_locstream(_RC)

    ! -- chunk
    sampler%LSF = LocStreamFactory(lons_chunk,lats_chunk,_RC)
    sampler%LS_chunk = sampler%LSF%create_locstream_on_proc(_RC)

    ! -- distributed
    call ESMF_FieldBundleGet(bundle,grid=grid,_RC)
    sampler%LS_ds = sampler%LSF%create_locstream_on_proc(grid=grid,_RC)
    
    !
    ! init ofile
    sampler%ofile=''
    sampler%obs_written=0

    _RETURN(_SUCCESS)
  end function new_StationSampler_readfile


  subroutine add_metadata_route_handle (this,items,bundle,timeInfo,vdata,rc)
    class(StationSampler),  intent(inout)       :: this
    type(GriddedIOitemVector), optional, intent(inout) :: items
    type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
    type(TimeData), optional, intent(inout)           :: timeInfo
    type(VerticalData), optional, intent(inout) :: vdata
    integer, optional, intent(out)              :: rc

    type(variable)   :: v
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
    integer          :: i

    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: var_name, long_name, units, vdims

    type(ESMF_Field) :: src_field, chunk_field
    real(REAL32), pointer :: pt1(:), pt2(:)

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
    call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC) ! specify lev in fmd
    do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
    if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%get_interpolating_variable(this%bundle,_RC)
    endif

    call timeInfo%add_time_to_metadata(this%metadata,_RC) ! specify time in fmd
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
    !      create varible with names in metadata; see create_metadata_variable
    !
    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
!!            print*, 'list item%xname', trim(item%xname)
       if (item%itemType == ItemTypeScalar) then
          call this%create_variable(item%xname,_RC)
       else if (item%itemType == ItemTypeVector) then
          call this%create_variable(item%xname,_RC)
          call this%create_variable(item%yname,_RC)
       end if
       call iter%next()
    enddo


    !__ 3. route handle  CS --> LS_ds
    !
    call ESMF_FieldBundleGet(bundle,grid=grid,_RC)
    this%regridder = LocStreamRegridder(grid,this%LS_ds,_RC)

    !__ 4. route handle  LS_ds --> LS_chunk
    !    
    src_field = ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
    chunk_field = ESMF_FieldCreate(this%LS_chunk,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
    call ESMF_FieldGet( src_field, localDE=0, farrayPtr=pt1, _RC )
    call ESMF_FieldGet( chunk_field, localDE=0, farrayPtr=pt2, _RC )
    pt1=0.0
    pt2=0.0
    call ESMF_FieldRedistStore(src_field,chunk_field,this%RH,_RC)
    call ESMF_FieldDestroy(src_field,noGarbage=.true.,_RC)
    call ESMF_FieldDestroy(chunk_field,noGarbage=.true.,_RC)
    
    _RETURN(_SUCCESS)
  end subroutine add_metadata_route_handle


  subroutine  create_metadata_variable(this,vname,rc)
    class(HistoryTrajectory), intent(inout) :: this
    character(len=*), intent(in)            :: vname
    integer, optional, intent(out)          :: rc
        
    type(ESMF_Field) :: field
    type(variable) :: v
    logical :: is_present
    integer :: field_rank, status
    character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
    integer :: k, ig

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
       vdims = this%index_name_x
    else if (field_rank==3) then
       vdims = trim(this%index_name_x)//",lev"
    end if

    if (field_rank==2) then
       vdims = "station_index,time"
       v = variable(type=PFIO_REAL32,dimensions=trim(vdims),chunksizes=[nstation,1])
    else if (field_rank==3) then
       vdims = "lev,station_index,time"
       call ESMF_FieldGet(field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
       v = variable(type=PFIO_REAL32,dimensions=trim(vdims),chunksizes=[ub(1)-lb(1)+1,1,1])
    end if

    call v%add_attribute('units',trim(units))
    call v%add_attribute('long_name',trim(long_name))
    call v%add_attribute('missing_value',MAPL_UNDEF)
    call v%add_attribute('_FillValue',MAPL_UNDEF)
    call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
    call this%metadata%add_variable(trim(var_name),v,_RC)

    _RETURN(_SUCCESS)
  end subroutine create_metadata_variable



  subroutine append_file(this,current_time,rc)
    class(StationSampler), intent(inout) :: this
    type(ESMF_Time), intent(in) :: current_time
    integer, optional, intent(out) :: rc
    !
    integer :: status
    integer :: fieldCount
    integer :: ub(1), lb(1)
    type(ESMF_Field) :: src_field,dst_field
    real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
    real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
    real(kind=REAL32), allocatable :: arr(:,:)
    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: xname
    real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)
    integer :: i, rank
    integer :: nx, nz

    type(ESMF_VM) :: vm
    integer :: mypet, petcount, mpic
    integer :: nx, nx_sum
    integer :: n0
    integer :: arr(1)
    integer :: sec
    integer, allocatable :: ix(:) !  counter for each obs(k)%nobs_epoch
    integer :: nx2
    logical :: EX ! file
    logical :: zero_obs
    integer, allocatable :: sendcount(:), displs(:)
    integer :: recvcount
    integer :: is, ie, ierr
    integer :: M, N, ip
         
    
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


    !__ 2. regrid + put_var: ungridded_dim from src to dst [regrid]
    !

    lm = this%vdata%lm
    field_2d_rt = ESMF_FieldCreate (this%LS_rt, name='field_2d_rt', typekind=ESMF_TYPEKIND_R4, _RC)
    field_3d_rt = ESMF_FieldCreate (this%LS_rt, name='field_3d_rt', typekind=ESMF_TYPEKIND_R4, &
         gridToFieldMap=[1],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
    
    field_2d_chunk = ESMF_FieldCreate (this%LS_chunk, name='field_2d_chunk', typekind=ESMF_TYPEKIND_R4, _RC)
    field_3d_chunk = ESMF_FieldCreate (this%LS_chunk, name='field_3d_chunk', typekind=ESMF_TYPEKIND_R4, &
         gridToFieldMap=[1],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
        
    !   caution about zero-sized array for MPI
    !   redist 
    nx_sum = this%nstation
    call ESMF_VMGetCurrent(vm,_RC)
    call ESMF_VMGet(vm, mpiCommunicator=mpic, petCount=petCount, localPet=mypet, _RC)

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


    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then


       call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
       call ESMF_FieldGet(src_field,rank=rank,_RC)
       if (rank==2) then
          call ESMF_FieldGet(src_field,farrayptr=p_src_2d,_RC)
          dst_field = ESMF_FieldCreate(this%LS_ds,name=xname, &
               typekind=ESMF_TYPEKIND_R4,_RC)
          call ESMF_FieldGet(dst_field,farrayptr=p_dst_2d,_RC)
          call this%regridder%regrid(p_src_2d,p_dst_2d,_RC)

          call ESMF_FieldGet(field_2d_chunk, localDE=0, farrayPtr=p_chunk_2d, _RC )
          call ESMF_FieldRedist(dst_field, field_2d_chunk, this%RH, _RC )
          call MPI_gatherv ( p_chunk_2d, nsend, MPI_REAL, &
               p_rt_2d, recvcount, displs, MPI_REAL,&
               iroot, mpic, ierr )

          if (mapl_am_i_root()) then
             call this%formatter%put_var(xname,p_rt_2d,&
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
          dst_field = ESMF_FieldCreate(this%LS_ds,name=xname,&
               typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
          call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,_RC)
          call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)

          call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,_RC)
          p_src= reshape(p_acc_3d,shape(p_src), order=[2,1])
          call ESMF_FieldRegrid(src_field,dst_field,RH,_RC)

          if (mapl_am_i_root()) then
             nx=size(p_dst_3d,1); nz=size(p_dst_3d,2); allocate(arr(nz, nx), _STAT)
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

    

          call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
          call ESMF_FieldGet(src_field,rank=rank,_RC)
          if (rank==1) then
             call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_2d, _RC )
             call ESMF_FieldGet( acc_field_2d_chunk, localDE=0, farrayPtr=p_acc_chunk_2d, _RC )
             call ESMF_FieldRedist( acc_field,  acc_field_2d_chunk, RH, _RC )
             call MPI_gatherv ( p_acc_chunk_2d, nsend, MPI_REAL, &
                  p_acc_rt_2d, recvcount, displs, MPI_REAL,&
                  iroot, mpic, ierr )
             
             
    

    
    call ESMF_FieldBundleGet(this%bundle, fieldCount=fieldCount, _RC)
    allocate (fieldNameList(fieldCount), _STAT)
    call ESMF_FieldBundleGet(this%bundle, fieldNameList=fieldNameList, _RC)
    do i=1, fieldCount
       xname=trim(fieldNameList(i))


       deallocate (fieldNameList)
    _RETURN(_SUCCESS)
  end subroutine append_file


  subroutine create_file_handle(this,filename,rc)
    class(StationSampler), intent(inout) :: this
    character(len=*), intent(inout) :: filename  ! for ouput nc
    integer, optional, intent(out) :: rc
    type(variable) :: v
    integer :: status, j

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
    call this%formatter%put_var('station_name',this%station_name,_RC)

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
    integer :: status
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

    var => this%metadata%get_variable('time',_RC)
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
    _RETURN(_SUCCESS)
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
    _ASSERT (i>0, 'not CSV format')
    _ASSERT (j>0, 'CSV format: find only 1 comma, should be > 1')
    j=i+j

    read(line(1:i-1), '(a100)', iostat=ios)  name
    _ASSERT (ios==0, 'read error')
    k=index(line(i+1:j-1), '.')
    if (k > 0) then
       read(line(i+1:j-1), *, iostat=ios) lon
    else
       read(line(i+1:j-1), *, iostat=ios) i
       lon = i
    endif
    _ASSERT (ios==0, 'read error')


    k=index(line(j+1:), '.')
    if (k > 0) then
       read(line(j+1:), *, iostat=ios) lat
    else
       read(line(j+1:), *, iostat=ios) i
       lat = i
    endif
    _ASSERT (ios==0, 'read error')

    !!write(6,*) trim(name), lon, lat
    _RETURN(_SUCCESS)

  end subroutine CSV_read_line_with_CH_I_R

end module StationSamplerMod

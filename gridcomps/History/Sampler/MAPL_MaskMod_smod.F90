#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

submodule (MaskSamplerMod)  MaskSampler_implement
  implicit none
contains

module function MaskSampler_from_config(config,string,clock,GENSTATE,rc) result(mask)
  use BinIOMod
  use pflogger, only         :  Logger, logging
  type(MaskSampler) :: mask
  type(ESMF_Config), intent(inout)        :: config
  character(len=*),  intent(in)           :: string
  type(ESMF_Clock),  intent(in)           :: clock
  type(MAPL_MetaComp), pointer, intent(in), optional  :: GENSTATE
  integer, optional, intent(out)          :: rc

  type(ESMF_Time)            :: currTime
  type(ESMF_TimeInterval)    :: epoch_frequency
  type(ESMF_TimeInterval)    :: obs_time_span
  integer                    :: time_integer, second
  integer                    :: status
  character(len=ESMF_MAXSTR) :: STR1, line
  character(len=ESMF_MAXSTR) :: symd, shms
  integer                    :: nline, col
  integer, allocatable       :: ncol(:)
  character(len=ESMF_MAXSTR), allocatable :: word(:)
  integer                    :: nobs, head, jvar
  logical                    :: tend
  integer                    :: i, j, k, M
  integer                    :: count
  integer                    :: unitr, unitw
  type(Logger), pointer :: lgr

  mask%clock=clock
  mask%grid_file_name=''
  if (present(GENSTATE)) mask%GENSTATE => GENSTATE

  call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
  if (mapl_am_I_root()) write(6,*) 'string', string

  call ESMF_ConfigGetAttribute(config, value=mask%grid_file_name,label=trim(string)//'obs_files:',    default="",  _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%index_name_x,  label=trim(string)//'index_name_x:', default="x", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%index_name_y,  label=trim(string)//'index_name_y:', default="y", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%var_name_x,    label=trim(string)//'var_name_x:',   default="x", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%var_name_y,    label=trim(string)//'var_name_y:',   default="y", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%var_name_proj, label=trim(string)//'var_name_proj:',default="",  _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%att_name_proj, label=trim(string)//'att_name_proj:',default="",  _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%thin_factor,   label=trim(string)//'thin_factor:',  default=-1,  _RC)

  if (mapl_am_I_root()) write(6,*) 'thin_factor:', mask%thin_factor
  call ESMF_ConfigGetAttribute(config, value=STR1, label=trim(string)//'obs_file_begin:', default="", _RC)
  if (trim(STR1)=='') then
     mask%obsfile_start_time = currTime
     call ESMF_TimeGet(currTime, timestring=STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_begin missing, default = currTime :', trim(STR1)
     endif
  else
     call ESMF_TimeSet(mask%obsfile_start_time, STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_begin provided: ', trim(STR1)
     end if
  end if

  call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
       label=trim(string) // 'obs_file_end:', _RC)
  if (trim(STR1)=='') then
     call ESMF_TimeIntervalSet(obs_time_span, d=14, _RC)
     mask%obsfile_end_time = mask%obsfile_start_time + obs_time_span
     call ESMF_TimeGet(mask%obsfile_end_time, timestring=STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_end   missing, default = begin+14D:', trim(STR1)
     endif
  else
     call ESMF_TimeSet(mask%obsfile_end_time, STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_end provided:', trim(STR1)
     end if
  end if

  call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
       label=trim(string) // 'obs_file_interval:', _RC)
  _ASSERT(STR1/='', 'fatal error: obs_file_interval not provided in RC file')
  if (mapl_am_I_root()) write(6,105) 'obs_file_interval:', trim(STR1)

  i= index( trim(STR1), ' ' )
  if (i>0) then
     symd=STR1(1:i-1)
     shms=STR1(i+1:)
  else
     symd=''
     shms=trim(STR1)
  endif
  call convert_twostring_2_esmfinterval (symd, shms,  mask%obsfile_interval, _RC)

  mask%is_valid = .true.
  mask%use_pfio = .false.   ! activate in set_param

  _RETURN(_SUCCESS)

105 format (1x,a,2x,a)
106 format (1x,a,2x,i8)
end function MaskSampler_from_config


   !
   !-- integrate both initialize and reinitialize
   !
module subroutine initialize(this,duration,frequency,items,bundle,timeInfo,vdata,global_attributes,reinitialize,rc)
   class(MaskSampler), intent(inout) :: this
   integer, intent(in) :: duration
   integer, intent(in) :: frequency
   type(GriddedIOitemVector), optional, intent(inout) :: items
   type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
   type(TimeData), optional, intent(inout)           :: timeInfo
   type(VerticalData), optional, intent(inout)       :: vdata
   type(StringStringMap), target, intent(in), optional :: global_attributes
   logical, optional, intent(in)           :: reinitialize
   integer, optional, intent(out)          :: rc

   integer :: status
   type(ESMF_Grid) :: grid
   type(variable) :: v
   type(GriddedIOitemVectorIterator) :: iter
   type(GriddedIOitem), pointer :: item
   type(ESMF_Time)            :: currTime
   integer :: n1, n2, k, j
   integer :: ic_2d, ic_3d, rank
   type(ESMF_Field) :: src_field


   if (.not. present(reinitialize)) then
      if(present(bundle))   this%bundle=bundle
      if(present(items))    this%items=items
      if(present(timeInfo)) this%timeinfo=timeInfo
      if(present(vdata)) then
         this%vdata=vdata
      else
         this%vdata=VerticalData(_RC)
      end if
   end if
   _ASSERT(present(global_attributes), 'PFIO needs global_attributes')


!   this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
!   if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

   this%obs_written = 0
   call this%create_Geosat_grid_find_mask(_RC)
   call this%create_metadata(global_attributes,_RC)
   n1 = MAPL_nsecf( duration )
   n2 = MAPL_nsecf( frequency )
   _ASSERT (n2>0, "list%frequency ==0, fail!")
   this%tmax =  n1/n2

   if (mapl_am_i_root()) write(6,*) 'mask smod init: af metadata'   

    if (this%use_pfio) then
       ic_2d=0
       ic_3d=0
       iter = this%items%begin()
       do while (iter /= this%items%end())
          item => iter%get()
          if (mapl_am_i_root()) write(6,*) 'mask smod init: item 1'
          
          if (item%itemType == ItemTypeScalar) then
             if (mapl_am_i_root()) write(6,*) 'mask smod init: item%xname:', trim(item%xname)
             
             call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
             call ESMF_FieldGet(src_field,rank=rank,_RC)
             if (rank==2) then
                ic_2d = ic_2d + 1
             else if (rank==3) then
                ic_3d = ic_3d + 1
             end if
          end if 
          call iter%next()         
       end do
       allocate ( this%var2d(ic_2d), _STAT )
       allocate ( this%var3d(ic_3d), _STAT )
       if (mapl_am_i_root()) write(6,*) 'mask smod init: af allocate var2d'

       
       do j=1, ic_2d
          if (mapl_am_i_root()) then
             allocate ( this%var2d(j)%array_x(this%npt_mask_tot), _STAT )
          else
             allocate ( this%var2d(j)%array_x(0), _STAT )
          end if
       end do
       do j=1, ic_3d
          if (mapl_am_i_root()) then
             allocate ( this%var3d(j)%array_zx(this%npt_mask_tot, this%vdata%lm), _STAT )
          else
             allocate ( this%var3d(j)%array_zx(0,0), _STAT )
          end if
       end do
    end if

   if (mapl_am_i_root()) write(6,*) 'mask smod init: af this%var3d'       
   !
   ! __ note thse large arrays should be deallocated in the future
   !

   _RETURN(_SUCCESS)

end subroutine initialize


module subroutine set_param(this,deflation,quantize_algorithm,quantize_level,chunking,&
     nbits_to_keep,regrid_method,itemOrder,write_collection_id,regrid_hints,oClients,rc)
  class (MaskSampler), intent(inout) :: this
  integer, optional, intent(in) :: deflation
  integer, optional, intent(in) :: quantize_algorithm
  integer, optional, intent(in) :: quantize_level
  integer, optional, intent(in) :: chunking(:)
  integer, optional, intent(in) :: nbits_to_keep
  integer, optional, intent(in) :: regrid_method
  logical, optional, intent(in) :: itemOrder
  integer, optional, intent(in) :: write_collection_id
  integer, optional, intent(in) :: regrid_hints
  type (ClientManager), optional, intent(in) :: oClients
  integer, optional, intent(out) :: rc
  integer :: status

  if (present(write_collection_id)) this%write_collection_id=write_collection_id
  if (present(itemOrder)) this%itemOrderAlphabetical = itemOrder
  if (present(oClients)) then
     this%use_pfio = .true.
     if (mapl_am_i_root()) then
        write(6, '(2x,a)') 'Mask sampler: use_pfio = .true.;  output to oserver'
     end if
  end if

!!  add later on
!!        if (present(regrid_method)) this%regrid_method=regrid_method
!!        if (present(nbits_to_keep)) this%nbits_to_keep=nbits_to_keep
!!        if (present(deflation)) this%deflateLevel = deflation
!!        if (present(quantize_algorithm)) this%quantizeAlgorithm = quantize_algorithm
!!        if (present(quantize_level)) this%quantizeLevel = quantize_level
!!        if (present(chunking)) then
!!           allocate(this%chunking,source=chunking,stat=status)
!!           _VERIFY(status)
!!        end if
!!        if (present(regrid_hints)) this%regrid_hints = regrid_hints

  _RETURN(ESMF_SUCCESS)

end subroutine set_param


module subroutine  create_metadata(this,global_attributes,rc)
    class(MaskSampler), intent(inout) :: this
    type(StringStringMap), target, intent(in) :: global_attributes
    integer, optional, intent(out)          :: rc

    type(variable)   :: v
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
    character(len=40) :: datetime_units

    type(StringStringMapIterator) :: s_iter
    type(stringVector) :: order
    integer :: metadataVarsSize
    character(len=:), pointer :: attr_name, attr_val

    !__ 1. metadata add_dimension,
    !     add_variable for time, mask_points, latlon,
    !

    if ( allocated (this%metadata) ) deallocate(this%metadata)
    allocate(this%metadata)

    call this%metadata%add_dimension('mask_index', this%npt_mask_tot)
    !- add time dimension to metadata
    call this%timeinfo%add_time_to_metadata(this%metadata,_RC)


    v = Variable(type=pFIO_REAL32, dimensions='mask_index')
    call v%add_attribute('long_name','longitude')
    call v%add_attribute('unit','degree_east')
    call this%metadata%add_variable('longitude',v)

    v = Variable(type=pFIO_REAL32, dimensions='mask_index')
    call v%add_attribute('long_name','latitude')
    call v%add_attribute('unit','degree_north')
    call this%metadata%add_variable('latitude',v)


    call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC) ! specify lev in fmd

    order = this%metadata%get_order(rc=status)
    _VERIFY(status)
    metadataVarsSize = order%size()


#undef this_debug
#define this_debug 1 
#ifdef this_debug

    !__ 2. filemetadata: extract field from bundle, add_variable to metadata
    !
    call ESMF_FieldBundleGet(this%bundle, fieldCount=fieldCount, _RC)
    allocate (fieldNameList(fieldCount), _STAT)
    call ESMF_FieldBundleGet(this%bundle, fieldNameList=fieldNameList, _RC)
    do i=1, fieldCount
       var_name=trim(fieldNameList(i))
       call ESMF_FieldBundleGet(this%bundle,var_name,field=field,_RC)
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

       if (field_rank==2) then
          vdims = "mask_index"
          v = variable(type=pfio_REAL32,dimensions=trim(vdims))
       else if (field_rank==3) then
          vdims = "mask_index,lev"
          v = variable(type=pfio_REAL32,dimensions=trim(vdims))
       end if

       call v%add_attribute('units',         trim(units))
       call v%add_attribute('long_name',     trim(long_name))
       call v%add_attribute('missing_value', MAPL_UNDEF)
       call v%add_attribute('_FillValue',    MAPL_UNDEF)
       call v%add_attribute('valid_range',   (/-MAPL_UNDEF,MAPL_UNDEF/))
       call this%metadata%add_variable(trim(var_name),v,_RC)
    end do
    deallocate (fieldNameList, _STAT)
#endif
    

    if (this%itemOrderAlphabetical) then
       call this%alphabatize_variables(metadataVarsSize,rc=status)
       _VERIFY(status)
    end if

    s_iter = global_attributes%begin()
    do while(s_iter /= global_attributes%end())
       attr_name => s_iter%key()
       attr_val => s_iter%value()
       call this%metadata%add_attribute(attr_name,attr_val,_RC)
       call s_iter%next()
    enddo

    ! To be added when values are available
    !v = Variable(type=pFIO_INT32, dimensions='mask_index')
    !call v%add_attribute('long_name','The Cubed Sphere Global Face ID')
    !call this%metadata%add_variable('mask_CS_Face_ID',v)
    !
    !v = Variable(type=pFIO_INT32, dimensions='mask_index')
    !call v%add_attribute('long_name','The Cubed Sphere Global Index I')
    !call this%metadata%add_variable('mask_CS_global_index_I',v)
    !
    !v = Variable(type=pFIO_INT32, dimensions='mask_index')
    !call v%add_attribute('long_name','The Cubed Sphere Global Index J')
    !call this%metadata%add_variable('mask_CS_global_index_J',v)


    _RETURN(_SUCCESS)
  end subroutine create_metadata


     module subroutine create_Geosat_grid_find_mask(this, rc)
       use pflogger, only: Logger, logging
       implicit none

       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc

       type(Logger), pointer :: lgr
       type(ESMF_routehandle) :: RH
       type(ESMF_Grid) :: grid
       integer :: mypet, petcount, mpic
       integer :: iroot, rootpet, ierr
       type (ESMF_LocStream) :: LS_rt
       type (ESMF_LocStream) :: LS_ds
       type (ESMF_LocStream) :: LS_chunk
       type (LocStreamFactory):: locstream_factory
       type (ESMF_Field) :: fieldA
       type (ESMF_Field) :: fieldB

       integer :: i, j, k, L
       integer :: n1, n2
       integer :: nx, ny, nx_sum
       integer :: nlon, nlat
       integer :: arr(1)
       integer :: len

       integer :: IM, JM, LM, COUNTS(3)
       type(ESMF_DistGrid) :: distGrid
       type(ESMF_DElayout) :: layout
       type(ESMF_VM) :: VM
       integer :: myid
       integer :: dimCount
       integer, allocatable :: II(:)
       integer, allocatable :: JJ(:)
       real(REAL64), allocatable :: obs_lons(:)
       real(REAL64), allocatable :: obs_lats(:)

       type (ESMF_Field) :: fieldI4
       type(ESMF_routehandle) :: RH_halo
       type(ESMF_Field) :: src_field,dst_field,acc_field
       integer :: useableHalo_width
       integer :: rank
       integer :: eLB(2), eUB(2)
       integer :: cLB(2), cUB(2)
       integer :: tLB(2), tUB(2)
       integer :: ecount(2)
       integer :: ccount(2)
       integer :: tcount(2)
       integer(ESMF_KIND_I4), pointer :: farrayPtr(:,:)
       real(ESMF_KIND_R8), pointer :: ptA(:) => NULL()
       real(ESMF_KIND_R8), pointer :: ptB(:) => NULL()

       character(len=50) :: filename
       integer :: unit
       integer :: ix, jx
       integer :: i_1, i_n, j_1, j_n
       real(REAL64), pointer :: x(:)
       real(REAL64), pointer :: y(:)
       real(REAL64) :: lambda0_deg, lambda0
       real(REAL64) :: x0, y0
       real(REAL64) :: lon0, lat0
       real(REAL64) :: lam_sat
       integer      :: mask0
       character(len=ESMF_MAXSTR) :: fn, key_x, key_y, key_p, key_p_att
       integer      :: Xdim_true, Ydim_true
       integer      :: Xdim_red, Ydim_red
       real(REAL64), allocatable :: lons(:), lats(:)
       real(REAL64), allocatable :: lons_ds(:), lats_ds(:)
       integer,      allocatable :: mask(:,:)

       real(ESMF_kind_R8), pointer :: lons_ptr(:,:), lats_ptr(:,:)
       integer :: nsend
       integer, allocatable :: recvcounts_loc(:), sendcounts_loc(:)
       integer, allocatable :: displs_loc(:)

       integer, allocatable :: sendcount(:), displs(:)
       integer :: recvcount
       integer :: M, N, ip
       integer :: nx2

       real(REAL64), allocatable :: lons_chunk(:)
       real(REAL64), allocatable :: lats_chunk(:)

       integer :: status, imethod


       lgr => logging%get_logger('HISTORY.sampler')

       ! Metacode:
       !   read ABI grid into  lons/lats, lons_chunk/lats_chunk
       !   gen LS_chunk and LS_ds with CS background grid
       !   find mask points on each PET with halo
       !   prepare recvcounts + displs for gatherv
       !

       call ESMF_VMGetCurrent(vm,_RC)
       call ESMF_VMGet(vm, mpiCommunicator=mpic, petcount=petcount, localpet=mypet, _RC)
       iroot = 0
       ip = mypet    ! 0 to M-1
       M = petCount

       if (ip==0) then
          write(6,*) 'ESMF_kind_R8, real32, real64, pfio_real32, pfio_real64'
          write(6,*) ESMF_kind_R8, real32, real64, pfio_real32, pfio_real64
       end if


       call MAPL_TimerOn(this%GENSTATE,"1_genABIgrid")
       if (mapl_am_i_root()) then
          ! __s1.  SAT file
          !
          fn    = this%grid_file_name
          key_x = this%var_name_x
          key_y = this%var_name_y
          key_p = this%var_name_proj
          key_p_att = this%att_name_proj
          call get_ncfile_dimension(fn,nlon=n1,nlat=n2,key_lon=key_x,key_lat=key_y,_RC)
          allocate (x(n1), y(n2), _STAT)
          call get_v1d_netcdf_R8_complete (fn, key_x, x, _RC)
          call get_v1d_netcdf_R8_complete (fn, key_y, y, _RC)
          call get_att_real_netcdf (fn, key_p, key_p_att, lambda0_deg, _RC)
          lam_sat = lambda0_deg * MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_CommsBcast(vm, DATA=n1, N=1, ROOT=MAPL_Root, _RC)
       call MAPL_CommsBcast(vm, DATA=n2, N=1, ROOT=MAPL_Root, _RC)
       if ( .NOT. mapl_am_i_root() )  allocate (x(n1), y(n2), _STAT)
       call MAPL_CommsBcast(vm, DATA=lam_sat, N=1, ROOT=MAPL_Root, _RC)
       call MAPL_CommsBcast(vm, DATA=x, N=n1, ROOT=MAPL_Root, _RC)
       call MAPL_CommsBcast(vm, DATA=y, N=n2, ROOT=MAPL_Root, _RC)

       !
       ! use thin_factor to reduce regridding matrix size
       !
       xdim_red  = n1 / this%thin_factor
       ydim_red  = n2 / this%thin_factor
       _ASSERT ( xdim_red * ydim_red > M, 'mask reduced points after thin_factor is less than Nproc!')

       ! get nx2: local on each ip
       nx2=0
       k=0
       do i=1, xdim_red
          do j=1, ydim_red
             k = k + 1
             if ( mod(k,M) == ip ) then
                x0 = x( i * this%thin_factor )
                y0 = y( j * this%thin_factor )
                call ABI_XY_2_lonlat (x0, y0, lam_sat, lon0, lat0, mask=mask0)
                if (mask0 > 0) then
                   nx2=nx2+1
                end if
             end if
          end do
       end do
       allocate (lons_chunk(nx2), lats_chunk(nx2), _STAT)

       ! get lons_chunk/...
       nx2 = 0
       k = 0
       do i=1, xdim_red
          do j=1, ydim_red
             k = k + 1
             if ( mod(k,M) == ip ) then
                x0 = x( i * this%thin_factor )
                y0 = y( j * this%thin_factor )
                call ABI_XY_2_lonlat (x0, y0, lam_sat, lon0, lat0, mask=mask0)
                if (mask0 > 0) then
                   nx2=nx2+1
                   lons_chunk(nx2) = lon0 * MAPL_RADIANS_TO_DEGREES
                   lats_chunk(nx2) = lat0 * MAPL_RADIANS_TO_DEGREES
                end if
             end if
          end do
       end do

       arr(1)=nx2
       call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx, &
            count=1, reduceflag=ESMF_REDUCE_SUM, _RC)


       ! gatherV for lons/lats
       if (mapl_am_i_root()) then
          allocate(lons(nx),lats(nx),_STAT)
       else
          allocate(lons(0),lats(0),_STAT)
       endif

       allocate( this%recvcounts(petcount), this%displs(petcount), _STAT )
       allocate( recvcounts_loc(petcount), displs_loc(petcount), _STAT )
       recvcounts_loc(:)=1
       displs_loc(1)=0
       do i=2, petcount
          displs_loc(i) = displs_loc(i-1) + recvcounts_loc(i-1)
       end do
       call MPI_gatherv ( nx2, 1, MPI_INTEGER, &
            this%recvcounts, recvcounts_loc, displs_loc, MPI_INTEGER,&
            iroot, mpic, ierr )
       _VERIFY(ierr)

       write(6,*) 'ip, nx2, this%recvcounts, recvcounts_loc, displs_loc'
       write(6,'(200i5)')  ip, nx2
       write(6,'(200i5)')  this%recvcounts
       write(6,'(200i5)')  recvcounts_loc
       write(6,'(200i5)')  displs_loc
       call MPI_Barrier(mpic,ierr)
       _VERIFY(ierr)


       if (.not. mapl_am_i_root()) then
          this%recvcounts(:) = 0
       end if
       this%displs(1)=0
       do i=2, petcount
          this%displs(i) = this%displs(i-1) + this%recvcounts(i-1)
       end do

       nsend = nx2
       call MPI_gatherv ( lons_chunk, nsend, MPI_REAL8, &
            lons, this%recvcounts, this%displs, MPI_REAL8,&
            iroot, mpic, ierr )
       _VERIFY(ierr)
       call MPI_gatherv ( lats_chunk, nsend, MPI_REAL8, &
            lats, this%recvcounts, this%displs, MPI_REAL8,&
            iroot, mpic, ierr )
       _VERIFY(ierr)

       !ygyu
       if (mapl_am_I_root()) write(6,*) 'nobs tot :', nx
       if (mapl_am_I_root()) write(6,*) 'ck1'

       deallocate (this%recvcounts, this%displs, _STAT)
       deallocate (recvcounts_loc, displs_loc, _STAT)
       deallocate (x, y, _STAT)
       call MAPL_TimerOff(this%GENSTATE,"1_genABIgrid")


       ! __ s2. set distributed LS
       !
       call MAPL_TimerOn(this%GENSTATE,"2_ABIgrid_LS")

       ! -- root
       locstream_factory = LocStreamFactory(lons,lats,_RC)
       LS_rt = locstream_factory%create_locstream(_RC)

       ! -- proc
       locstream_factory = LocStreamFactory(lons_chunk,lats_chunk,_RC)
       LS_chunk = locstream_factory%create_locstream_on_proc(_RC)

       ! -- distributed with background grid
       call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
       LS_ds = locstream_factory%create_locstream_on_proc(grid=grid,_RC)

       fieldA = ESMF_FieldCreate (LS_chunk, name='A', typekind=ESMF_TYPEKIND_R8, _RC)
       fieldB = ESMF_FieldCreate (LS_ds, name='B', typekind=ESMF_TYPEKIND_R8, _RC)
       call ESMF_FieldGet( fieldA, localDE=0, farrayPtr=ptA)
       call ESMF_FieldGet( fieldB, localDE=0, farrayPtr=ptB)

       ptA(:) = lons_chunk(:)
       call ESMF_FieldRedistStore (fieldA, fieldB, RH, _RC)
       call MPI_Barrier(mpic,ierr)
       _VERIFY(ierr)
       call ESMF_FieldRedist      (fieldA, fieldB, RH, _RC)
       lons_ds = ptB

       ptA(:) = lats_chunk(:)
       call MPI_Barrier(mpic,ierr)
       _VERIFY(ierr)
       call ESMF_FieldRedist      (fieldA, fieldB, RH, _RC)
       lats_ds = ptB

!!       write(6,*)  'ip, size(lons_ds)=', mypet, size(lons_ds)
       if (mapl_am_I_root()) write(6,*) 'ck2'
       
       call MAPL_TimerOff(this%GENSTATE,"2_ABIgrid_LS")

       ! __ s3. find n.n. CS pts for LS_ds (halo)
       !
       call MAPL_TimerOn(this%GENSTATE,"3_CS_halo")
!       allocate (obs_lons( size(lons_ds)), _STAT)
!       allocate (obs_lats( size(lons_ds)), _STAT)
       obs_lons = lons_ds * MAPL_DEGREES_TO_RADIANS_R8
       obs_lats = lats_ds * MAPL_DEGREES_TO_RADIANS_R8

!! ygyu debug
!!       nx = sizeof ( ptB ) / sizeof (ESMF_KIND_R8)
!!       nx = size (ptB, 1)
       nx = size ( lons_ds )

       call ESMF_FieldDestroy(fieldA,nogarbage=.true.,_RC)
       call ESMF_FieldDestroy(fieldB,nogarbage=.true.,_RC)
       call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)
       allocate ( II(nx), JJ(nx), _STAT )
       call MAPL_GetHorzIJIndex(nx,II,JJ,lonR8=obs_lons,latR8=obs_lats,grid=grid,_RC)
       call ESMF_VMBarrier (vm, _RC)

       !
       ! __  halo for mask
       !
       call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, _RC)
       IM= COUNTS(1)
       JM= COUNTS(2)
       LM= COUNTS(3)
       useableHalo_width = 1
       fieldI4 = ESMF_FieldCreate (grid, ESMF_TYPEKIND_I4, &
            totalLwidth=[useableHalo_width,useableHalo_width],&
            totalUwidth=[useableHalo_width,useableHalo_width], _RC)
       call ESMF_FieldGetBounds (fieldI4, &
            exclusiveLBound=eLB, exclusiveUBound=eUB, exclusiveCount=ecount, &
            totalLBound=tLB, totalUBound=tUB, totalCount=tcount, &
            computationalLBound=cLB, computationalUBound=cUB, computationalCount=ccount, &
            _RC)
       call ESMF_FieldGet (fieldI4, farrayPtr=farrayPtr,  _RC)
       farrayPtr(:,:) = 0
       do i=1, nx
          if ( II(i)>0 .AND. JJ(i)>0 ) then
             farrayPtr( II(i), JJ(i) ) = 1
          endif
       enddo

       call ESMF_FieldHaloStore (fieldI4, routehandle=RH_halo, _RC)
       call ESMF_FieldHalo (fieldI4, routehandle=RH_halo, _RC)

!       !
!       !-- print out eLB, eUB do they match 1:IM, JM?
!       !
!       write(6,*) 'IM,JM', IM,JM
!       write(6,*) 'eLB(1), eUB(1)', eLB(1), eUB(1)
!       write(6,*) 'eLB(2), eUB(2)', eLB(2), eUB(2)

       k=0
       do i=eLB(1), eUB(1)
          do j=eLB(2), eUB(2)
             if ( farrayPtr(i,j)==0 .AND. ( &
                  farrayPtr(i-1,j)==1 .OR. &
                  farrayPtr(i+1,j)==1 .OR. &
                  farrayPtr(i,j-1)==1 .OR. &
                  farrayPtr(i,j+1)==1 )  ) then
                farrayPtr(i,j) = -1
             end if
             if (farrayPtr(i,j)/=0) k=k+1
          end do
       end do
       allocate( mask(IM, JM), _STAT)
       mask(1:IM, 1:JM) = abs(farrayPtr(1:IM, 1:JM))

       this%npt_mask = k    ! # of masked pts on CS grid
       allocate( this%index_mask(2,k), _STAT )
       arr(1)=k
       call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=this%npt_mask_tot, &
            count=1, reduceflag=ESMF_REDUCE_SUM, _RC)

       k=0
       do i=1, IM
          do j=1, JM
             if ( mask(i,j)==1 ) then
                k=k+1
                this%index_mask(1,k) = i
                this%index_mask(2,k) = j
             end if
          end do
       end do
       call MAPL_TimerOff(this%GENSTATE,"3_CS_halo")
       if (mapl_am_I_root()) write(6,*) 'ck3'

       ! ----
       !  regridding is replaced by
       !  - selecting masked data on PET
       !  - mpi_gatherV
       !

       call MAPL_TimerOn(this%GENSTATE,"4_gatherV")

       ! __ s4.1 find this%lons/lats on root for NC output
       !
       call ESMF_GridGetCoord (grid, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lons_ptr, _RC)
       call ESMF_GridGetCoord (grid, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats_ptr, _RC)
       deallocate (lons, lats, _STAT)
       allocate (lons(this%npt_mask), lats(this%npt_mask), _STAT)
       do i=1, this%npt_mask
          ix=this%index_mask(1,i)
          jx=this%index_mask(2,i)
          lons(i) = lons_ptr (ix, jx)
          lats(i) = lats_ptr (ix, jx)
       end do

       iroot=0
       if (mapl_am_i_root()) then
          allocate (this%lons(this%npt_mask_tot), this%lats(this%npt_mask_tot), _STAT)
       else
          allocate (this%lons(0), this%lats(0), _STAT)
       end if
       if (mapl_am_I_root()) write(6,*) 'ck4.1'

       
       ! __ s4.2  find this%recvcounts / this%displs
       !
       allocate( this%recvcounts(petcount), this%displs(petcount), _STAT )
       allocate( recvcounts_loc(petcount), displs_loc(petcount), _STAT )
       recvcounts_loc(:)=1
       displs_loc(1)=0
       do i=2, petcount
          displs_loc(i) = displs_loc(i-1) + recvcounts_loc(i-1)
       end do
       call MPI_gatherv ( this%npt_mask, 1, MPI_INTEGER, &
            this%recvcounts, recvcounts_loc, displs_loc, MPI_INTEGER,&
            iroot, mpic, ierr )
       _VERIFY(ierr)
       !
       ! set nonroot to zero for s4.3
       if (.not. mapl_am_i_root()) then
          this%recvcounts(:) = 0
       end if
       this%displs(1)=0
       do i=2, petcount
          this%displs(i) = this%displs(i-1) + this%recvcounts(i-1)
       end do

       ! __ s4.3  gatherv lons/lats
       !
       nsend=this%npt_mask
       call MPI_gatherv ( lons, nsend, MPI_REAL8, &
            this%lons, this%recvcounts, this%displs, MPI_REAL8,&
            iroot, mpic, ierr )
       _VERIFY(ierr)
       call MPI_gatherv ( lats, nsend, MPI_REAL8, &
            this%lats, this%recvcounts, this%displs, MPI_REAL8,&
            iroot, mpic, ierr )
       _VERIFY(ierr)

       call MAPL_TimerOff(this%GENSTATE,"4_gatherV")
       if (mapl_am_I_root()) write(6,*) 'ck4.3'


!         __ note: s4.4 can be used in the future for pfio
!         __       for now keep it simple
!       ! __ s4.4  find (i1,in) for masked array
!       write(6,*) 'ip, this%npt_mask, this%recvcounts, this%displs'
!       write(6,'(200i10)')  ip, this%npt_mask
!       write(6,'(200i10)')  this%recvcounts
!       write(6,'(200i10)')  this%displs
!       call MPI_Barrier(mpic,ierr)
!       _VERIFY(ierr)

       if (mapl_am_i_root()) then
          print*, 'this%npt_mask_tot=', this%npt_mask_tot
          allocate (this%lons_deg(this%npt_mask_tot), this%lats_deg(this%npt_mask_tot), _STAT)
          this%lons_deg = this%lons * MAPL_RADIANS_TO_DEGREES
          this%lats_deg = this%lats * MAPL_RADIANS_TO_DEGREES
       else
          allocate (this%lons_deg(0), this%lats_deg(0), _STAT)
       end if
!!       write(6,'(2x,a,2x,i5,2x,1000f12.2)') 'ip, lons_deg', ip, this%lons_deg
!!       write(6,'(2x,a,2x,i5,2x,1000f12.2)') 'ip, lats_deg', ip, this%lats_deg

       
!!!       call MAPL_CommsBcast(vm, DATA=, N=1, ROOT=MAPL_Root, _RC)
!       allocate (sendcounts_loc(petcount))
!       do i=1, petcount
!          displs_loc(i)=i-1
!          sendcounts_loc(i)=1
!       enddo
!
!       call  MPI_Scatterv( this%displs, sendcounts_loc, displs_loc, MPI_INTEGER, &
!            this%i1, 1, MPI_INTEGER, iroot, mpic, ierr)
!       if (this%npt_mask > 0) then
!          this%i1 = this%i1 + 1       ! shift from 0 to 1
!          this%in =  this%i1 + this%npt_mask - 1
!       else
!          this%i1 = 0
!          this%in = 0
!       end if
!
!       write(6,'(2x,a,2x,200i10)')  'ip, this%npt_mask, this%i1, in:', &
!            ip, this%npt_mask, this%i1, this%in
!       call MPI_Barrier(mpic,ierr)


       _RETURN(_SUCCESS)
     end subroutine create_Geosat_grid_find_mask


 module subroutine regrid_append_file(this,current_time,filename,oClients,rc)
    implicit none
    class(MaskSampler), intent(inout) :: this
    type(ESMF_Time), intent(inout)          :: current_time
    character(len=*), intent(in) :: filename
    type (ClientManager), target, optional, intent(inout) :: oClients
    integer, optional, intent(out)          :: rc
    !
    integer :: status
    integer :: fieldCount
    integer :: ub(1), lb(1)
    type(ESMF_Field) :: src_field,dst_field
    real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
    real(kind=REAL32), allocatable :: p_dst_3d(:),p_dst_2d(:)
    real(kind=REAL32), allocatable :: p_dst_3d_full(:),p_dst_2d_full(:)
    real(kind=REAL32), allocatable :: arr(:,:)
    character(len=ESMF_MAXSTR), allocatable ::  fieldNameList(:)
    character(len=ESMF_MAXSTR) :: xname
    real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)
    integer :: i, j, k, rank
    integer :: nx, nz
    integer :: ix, iy, m
    integer :: mypet, petcount, nsend
    integer :: iroot, ierr
    integer :: mpic
    integer :: ic_2d, ic_3d
    integer, allocatable :: recvcounts_3d(:)
    integer, allocatable :: displs_3d(:)
    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    type(ESMF_VM) :: vm
    type(ArrayReference) :: ref

    if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt0'        

    this%obs_written=this%obs_written+1

    ! -- fixed for all fields
    call ESMF_VMGetCurrent(vm,_RC)
    call ESMF_VMGet(vm, mpiCommunicator=mpic, petcount=petcount, localpet=mypet, _RC)
    iroot=0
    nx = this%npt_mask
    nz = this%vdata%lm
    allocate(p_dst_2d (nx), _STAT)
    allocate(p_dst_3d (nx * nz), _STAT)
    if (mapl_am_i_root()) then
       allocate ( p_dst_2d_full (this%npt_mask_tot), _STAT )
       allocate ( p_dst_3d_full (this%npt_mask_tot * nz), _STAT )
    else
       allocate ( p_dst_2d_full (0), _STAT )
       allocate ( p_dst_3d_full (0), _STAT )
    end if
    allocate( recvcounts_3d(petcount), displs_3d(petcount), _STAT )
    recvcounts_3d(:) = nz * this%recvcounts(:)
    displs_3d(:)     = nz * this%displs(:)

    if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt1'    

    !__ 1. put_var: time variable
    !
    allocate( rtimes(1), _STAT )
    rtimes(1) = this%compute_time_for_current(current_time,_RC) ! rtimes: seconds since opening file
    if (this%use_pfio) then
       if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt1.2   write time'
       this%rtime = rtimes(1)*1.0
       ref = ArrayReference(this%rtime)
       call oClients%collective_stage_data(this%write_collection_id,trim(filename),'time', &
            ref,start=[1], global_start=[1], global_count=[1])
       call this%stage2DLatLon(trim(filename),oClients=oClients,_RC)
    else
       if (mapl_am_i_root()) then
          call this%formatter%put_var('time',rtimes(1:1),&
               start=[this%obs_written],count=[1],_RC)
          call this%formatter%put_var('longitude',this%lons_deg,_RC)
          call this%formatter%put_var('latitude',this%lats_deg,_RC)
       end if
    end if
 
    if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt2'       


    !__ 2. put_var: ungridded_dim from src to dst [use index_mask]
    !
    !   Currently mask only pickup values
    !   It does not support vertical regridding
    !
    !if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
    !   call this%vdata%setup_eta_to_pressure(_RC)
    !endif

    iter = this%items%begin()
    ic_2d = 0
    ic_3d = 0
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then
          call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
          call ESMF_FieldGet(src_field,rank=rank,_RC)
          if (rank==2) then
             call ESMF_FieldGet(src_field,farrayptr=p_src_2d,_RC)
             do j=1, nx
                ix = this%index_mask(1,j)
                iy = this%index_mask(2,j)
                p_dst_2d(j) = p_src_2d(ix, iy)
             end do
             nsend = nx
             call MPI_gatherv ( p_dst_2d, nsend, MPI_REAL, &
                  p_dst_2d_full, this%recvcounts, this%displs, MPI_REAL,&
                  iroot, mpic, status )
             _VERIFY(status)
             call MAPL_TimerOn(this%GENSTATE,"put2D")
             if (this%use_pfio) then
                ic_2d = ic_2d + 1
                if (mapl_am_i_root()) then
                   this%var2d(ic_2d)%array_x(1:this%npt_mask_tot) = p_dst_2d_full(1:this%npt_mask_tot)
                endif
                ref = ArrayReference(this%var2d(ic_2d)%array_x)
                call oClients%collective_stage_data(this%write_collection_id,trim(filename), item%xname, &
                     ref,start=[1], global_start=[1], global_count=[this%npt_mask_tot])
             else
                if (mapl_am_i_root()) then
                   call this%formatter%put_var(item%xname,p_dst_2d_full,&
                        start=[1,this%obs_written],count=[this%npt_mask_tot,1],_RC)
                end if
             end if
             call MAPL_TimerOff(this%GENSTATE,"put2D")
             if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt3'       
             
          else if (rank==3) then
             call ESMF_FieldGet(src_field,farrayptr=p_src_3d,_RC)
             call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
             _ASSERT (this%vdata%lm == (ub(1)-lb(1)+1), 'vertical level is different from CS grid')
             m=0
             do j=1, nx
                ix = this%index_mask(1,j)
                iy = this%index_mask(2,j)
                do k= lb(1), ub(1)
                   m = m + 1
                   p_dst_3d(m) = p_src_3d(ix, iy, k)
                end do
             end do
             !! write(6,'(2x,a,2x,i5,3x,10f8.1)') 'pet, p_dst_3d(j)', mypet, p_dst_3d(::10)
             nsend = nx * nz
             call MPI_gatherv ( p_dst_3d, nsend, MPI_REAL, &
                  p_dst_3d_full, recvcounts_3d, displs_3d, MPI_REAL,&
                  iroot, mpic, status )
             _VERIFY(status)
             call MAPL_TimerOn(this%GENSTATE,"put3D")

             if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt4.1'
             
             if (this%use_pfio) then
                ic_3d = ic_3d + 1
                if (mapl_am_i_root()) then
                   this%var3d(ic_3d)%array_zx(1:nz, 1:this%npt_mask_tot) = &
                        reshape(p_dst_3d_full,[nz,this%npt_mask_tot],order=[1,2])                        
                   this%var3d(ic_3d)%array_zx(1:nz, 1:this%npt_mask_tot) = 99.0
                endif
                ref = ArrayReference(this%var3d(ic_3d)%array_zx)
                if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt4.1.1'                             
                call oClients%collective_stage_data(this%write_collection_id,trim(filename), item%xname, &
                     ref,start=[1,1], global_start=[1,1], global_count=[nz, this%npt_mask_tot])
                     ! 2d: ref,start=[1], global_start=[1], global_count=[this%npt_mask_tot])                

                if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt4.1.2'                             
             else
                if (mapl_am_i_root()) then
                   allocate(arr(nz, this%npt_mask_tot), _STAT)
                   arr=reshape(p_dst_3d_full,[nz,this%npt_mask_tot],order=[1,2])
                   call this%formatter%put_var(item%xname,arr,&
                        start=[1,1,this%obs_written],count=[nz,this%npt_mask_tot,1],_RC)
                   !note:     lev,station,time
                   deallocate(arr, _STAT)
                end if
             end if
             call MAPL_TimerOff(this%GENSTATE,"put3D")
             if (mapl_am_I_root()) write(6,*) 'ck  regrid append pt4.2'             

          else
             _FAIL('grid2LS regridder: rank > 3 not implemented')
          end if
       end if

       call iter%next()
    end do

    _RETURN(_SUCCESS)
  end subroutine regrid_append_file


  module function compute_time_for_current(this,current_time,rc) result(rtime)
    use  MAPL_NetCDF, only : convert_NetCDF_DateTime_to_ESMF
    class(MaskSampler), intent(inout) :: this
    type(ESMF_Time), intent(in) :: current_time
    integer, optional, intent(out) :: rc
    real(kind=ESMF_KIND_R8) :: rtime

    integer :: status
    type(ESMF_TimeInterval) :: t_interval
    class(Variable), pointer :: var
    type(Attribute), pointer :: attr
    class(*), pointer :: pTimeUnits
    character(len=ESMF_MAXSTR) :: datetime_units
    character(len=ESMF_MAXSTR) :: tunit
    type(ESMF_time), allocatable :: esmf_time_1d(:)
    real(kind=ESMF_KIND_R8), allocatable :: rtime_1d(:)

    var => this%metadata%get_variable('time',_RC)
    attr => var%get_attribute('units')
    ptimeUnits => attr%get_value()
    select type(pTimeUnits)
    type is (character(*))
       datetime_units = ptimeUnits
    class default
       _FAIL("Time unit must be character")
    end select
    allocate (  esmf_time_1d(1), rtime_1d(1), _STAT )
    esmf_time_1d(1)= current_time
    call time_ESMF_to_real ( rtime_1d, esmf_time_1d, datetime_units, _RC )
    rtime =  rtime_1d(1)

    _RETURN(_SUCCESS)
  end function compute_time_for_current

  module subroutine stage2dlatlon(this,filename,oClients,rc)
    implicit none

    class(MaskSampler), intent(inout) :: this
    character(len=*), intent(in) :: fileName
    type (ClientManager), optional, target, intent(inout) :: oClients
    integer, optional, intent(out) :: rc

    integer, allocatable :: local_start(:)
    integer, allocatable :: global_start(:)
    integer, allocatable :: global_count(:)
    integer :: n
    type(ArrayReference), target :: ref
    integer :: status

    ! Note: we have already gatherV to root the lon/lat
    !       in sub. create_Geosat_grid_find_mask
    !
    if (mapl_am_i_root()) then
       allocate(local_start,source=[1])
       allocate(global_start,source=[1])
       allocate(global_count,source=[this%npt_mask_tot])
    else
       allocate(local_start,source=[0])
       allocate(global_start,source=[0])
       allocate(global_count,source=[0])
    end if


    !! write(6,'(100f6.1,2x)')  this%lons_deg(:)
    ref = ArrayReference(this%lons_deg)
    call oClients%collective_stage_data(this%write_collection_id,trim(filename),'longitude', &
         ref,start=local_start, global_start=global_start, global_count=global_count)

    ref = ArrayReference(this%lats_deg)
    call oClients%collective_stage_data(this%write_collection_id,trim(filename),'latitude', &
         ref,start=local_start, global_start=global_start, global_count=global_count)

    _RETURN(_SUCCESS)
 end subroutine stage2dlatlon


     module subroutine modifyTime(this, oClients, rc)
        class(MaskSampler), intent(inout) :: this
        type (ClientManager), optional, intent(inout) :: oClients
        integer, optional, intent(out) :: rc

        type(Variable) :: v
        type(StringVariableMap) :: var_map
        integer :: status

        if (this%timeInfo%is_initialized) then
           v = this%timeInfo%define_time_variable(_RC)
           call this%metadata%modify_variable('time',v,rc=status)
           _VERIFY(status)
           if (present(oClients)) then
              call var_map%insert('time',v)
              call oClients%modify_metadata(this%write_collection_id, var_map=var_map, rc=status)
              _VERIFY(status)
           end if
        else
           _FAIL("Time was not initialized for the GriddedIO class instance")
        end if
        _RETURN(ESMF_SUCCESS)

     end subroutine modifyTime



   module subroutine alphabatize_variables(this,nfixedVars,rc)
     class (masksampler), intent(inout) :: this
     integer, intent(in) :: nFixedVars
     integer, optional, intent(out) :: rc

     type(StringVector) :: order
     type(StringVector) :: newOrder
     character(len=:), pointer :: v1
     character(len=ESMF_MAXSTR) :: c1,c2
     character(len=ESMF_MAXSTR), allocatable :: temp(:)
     logical :: swapped
     integer :: n,i
     integer :: status

     order = this%metadata%get_order(rc=status)
     _VERIFY(status)
     n = Order%size()
     allocate(temp(nFixedVars+1:n))
     do i=1,n
        v1 => order%at(i)
        if ( i > nFixedVars) temp(i)=trim(v1)
     enddo

     swapped = .true.
     do while(swapped)
        swapped = .false.
        do i=nFixedVars+1,n-1
           c1 = temp(i)
           c2 = temp(i+1)
           if (c1 > c2) then
              temp(i+1)=c1
              temp(i)=c2
              swapped =.true.
           end if
        enddo
     enddo

     do i=1,nFixedVars
        v1 => Order%at(i)
        call newOrder%push_back(v1)
     enddo
     do i=nFixedVars+1,n
        call newOrder%push_back(trim(temp(i)))
     enddo
     call this%metadata%set_order(newOrder,rc=status)
     _VERIFY(status)
     deallocate(temp)

     _RETURN(_SUCCESS)

  end subroutine alphabatize_variables


module subroutine finalize(this,rc)
   class(MaskSampler), intent(inout) :: this
   integer, optional, intent(out)          :: rc
   type(GriddedIOitemVectorIterator) :: iter
   type(GriddedIOitem), pointer :: item
   type(ESMF_Field) :: src_field
   integer :: ic_2d, ic_3d, rank, j
   integer :: status


    if (this%use_pfio) then
       ic_2d=0
       ic_3d=0
       iter = this%items%begin()
       do while (iter /= this%items%end())
          item => iter%get()
          if (item%itemType == ItemTypeScalar) then
             call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
             call ESMF_FieldGet(src_field,rank=rank,_RC)
             if (rank==2) then
                ic_2d = ic_2d + 1
             else if (rank==3) then
                ic_3d = ic_3d + 1
             end if
          end if
          call iter%next()
       end do

       do j=1, ic_2d
          deallocate ( this%var2d(j)%array_x, _STAT )
       end do
       deallocate ( this%var2d, _STAT )
       do j=1, ic_3d
          deallocate ( this%var3d(j)%array_zx, _STAT )
       end do
       deallocate ( this%var3d, _STAT )
    end if

    _RETURN(_SUCCESS)
  end subroutine finalize

   end submodule MaskSampler_implement


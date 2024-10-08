#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

submodule (MaskSamplerGeosatMod)  MaskSamplerGeosat_implement
  implicit none
contains

module function MaskSamplerGeosat_from_config(config,string,clock,GENSTATE,rc) result(mask)
  use BinIOMod
  use pflogger, only         :  Logger, logging
  type(MaskSamplerGeosat) :: mask
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

  _RETURN(_SUCCESS)

105 format (1x,a,2x,a)
106 format (1x,a,2x,i8)
end function MaskSamplerGeosat_from_config


   !
   !-- integrate both initialize and reinitialize
   !
module subroutine initialize_(this,items,bundle,timeInfo,vdata,reinitialize,rc)
   class(MaskSamplerGeosat), intent(inout) :: this
   type(GriddedIOitemVector), optional, intent(inout) :: items
   type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
   type(TimeData), optional, intent(inout)           :: timeInfo
   type(VerticalData), optional, intent(inout)       :: vdata
   logical, optional, intent(in)           :: reinitialize
   integer, optional, intent(out)          :: rc

   integer :: status
   type(ESMF_Grid) :: grid
   type(variable) :: v
   type(GriddedIOitemVectorIterator) :: iter
   type(GriddedIOitem), pointer :: item
   type(ESMF_Time)            :: currTime
   integer :: k

   if (.not. present(reinitialize)) then
      if(present(bundle))   this%bundle=bundle
      if(present(items))    this%items=items
      if(present(timeInfo)) this%time_info=timeInfo
      if (present(vdata)) then
         this%vdata=vdata
      else
         this%vdata=VerticalData(_RC)
      end if
   end if

!   this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
!   if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

   this%ofile = ''
   this%obs_written = 0

   call this%create_grid(_RC)
   call this%add_metadata(_RC)

   _RETURN(_SUCCESS)

end subroutine initialize_


     module subroutine create_Geosat_grid_find_mask(this, rc)
       use pflogger, only: Logger, logging
       implicit none

       class(MaskSamplerGeosat), intent(inout) :: this
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
       integer, allocatable :: recvcounts_loc(:)
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

       ! get nx2
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


!!       if (mapl_am_I_root()) write(6,*) 'nobs tot :', nx

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

       call ESMF_FieldDestroy(fieldA,nogarbage=.true.,_RC)
       call ESMF_FieldDestroy(fieldB,nogarbage=.true.,_RC)
       call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)

       call MAPL_TimerOff(this%GENSTATE,"2_ABIgrid_LS")


       ! __ s3. find n.n. CS pts for LS_ds (halo)
       !
       call MAPL_TimerOn(this%GENSTATE,"3_CS_halo")
       obs_lons = lons_ds * MAPL_DEGREES_TO_RADIANS_R8
       obs_lats = lats_ds * MAPL_DEGREES_TO_RADIANS_R8
       nx = size ( lons_ds )
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

       this%npt_mask = k
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

       _RETURN(_SUCCESS)
     end subroutine create_Geosat_grid_find_mask


module subroutine  add_metadata(this,rc)
    class(MaskSamplerGeosat), intent(inout) :: this
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

    !__ 1. metadata add_dimension,
    !     add_variable for time, latlon, mask_points
    !
    call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC) ! specify lev in fmd
    call this%time_info%add_time_to_metadata(this%metadata,_RC)
    call this%metadata%add_dimension('mask_index', this%npt_mask_tot)

    v = Variable(type=pFIO_REAL64, dimensions='mask_index')
    call v%add_attribute('long_name','longitude')
    call v%add_attribute('unit','degree_east')
    call this%metadata%add_variable('longitude',v)

    v = Variable(type=pFIO_REAL64, dimensions='mask_index')
    call v%add_attribute('long_name','latitude')
    call v%add_attribute('unit','degree_north')
    call this%metadata%add_variable('latitude',v)

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
          vdims = "mask_index,time"
          v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
       else if (field_rank==3) then
          vdims = "lev,mask_index,time"
          call ESMF_FieldGet(field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
          v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
       end if
       call v%add_attribute('units',         trim(units))
       call v%add_attribute('long_name',     trim(long_name))
       call v%add_attribute('missing_value', MAPL_UNDEF)
       call v%add_attribute('_FillValue',    MAPL_UNDEF)
       call v%add_attribute('valid_range',   (/-MAPL_UNDEF,MAPL_UNDEF/))
       call this%metadata%add_variable(trim(var_name),v,_RC)
    end do
    deallocate (fieldNameList, _STAT)

    _RETURN(_SUCCESS)
  end subroutine add_metadata


 module subroutine regrid_append_file(this,current_time,rc)
    implicit none

    class(MaskSamplerGeosat), intent(inout) :: this
    type(ESMF_Time), intent(inout)          :: current_time
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
    integer, allocatable :: recvcounts_3d(:)
    integer, allocatable :: displs_3d(:)
    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    type(ESMF_VM) :: vm

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


    !__ 1. put_var: time variable
    !
    allocate( rtimes(1), _STAT )
    rtimes(1) = this%compute_time_for_current(current_time,_RC) ! rtimes: seconds since opening file
    if (mapl_am_i_root()) then
       call this%formatter%put_var('time',rtimes(1:1),&
            start=[this%obs_written],count=[1],_RC)
    end if


    !__ 2. put_var: ungridded_dim from src to dst [use index_mask]
    !
    !   Currently mask only pickup values
    !   It does not support vertical regridding
    !
    !if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
    !   call this%vdata%setup_eta_to_pressure(_RC)
    !endif

    iter = this%items%begin()
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
             if (mapl_am_i_root()) then
                call this%formatter%put_var(item%xname,p_dst_2d_full,&
                     start=[1,this%obs_written],count=[this%npt_mask_tot,1],_RC)
             end if
             call MAPL_TimerOff(this%GENSTATE,"put2D")
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
             if (mapl_am_i_root()) then
                allocate(arr(nz, this%npt_mask_tot), _STAT)
                arr=reshape(p_dst_3d_full,[nz,this%npt_mask_tot],order=[1,2])
                call this%formatter%put_var(item%xname,arr,&
                     start=[1,1,this%obs_written],count=[nz,this%npt_mask_tot,1],_RC)
                !note:     lev,station,time
                deallocate(arr, _STAT)
             end if
             call MAPL_TimerOff(this%GENSTATE,"put3D")
          else
             _FAIL('grid2LS regridder: rank > 3 not implemented')
          end if
       end if

       call iter%next()
    end do

    _RETURN(_SUCCESS)
  end subroutine regrid_append_file



  module subroutine create_file_handle(this,filename,rc)
    class(MaskSamplerGeosat), intent(inout) :: this
    character(len=*), intent(in)            :: filename
    integer, optional, intent(out)          :: rc
    type(variable) :: v
    integer :: status, j
    real(kind=REAL64), allocatable :: x(:)
    integer :: nx

    this%ofile = trim(filename)
    v = this%time_info%define_time_variable(_RC)
    call this%metadata%modify_variable('time',v,_RC)
    this%obs_written = 0

    if (.not. mapl_am_I_root()) then
       _RETURN(_SUCCESS)
    end if

    call this%formatter%create(trim(filename),_RC)
    call this%formatter%write(this%metadata,_RC)

    nx = size (this%lons)
    allocate ( x(nx), _STAT )
    x(:) = this%lons(:) * MAPL_RADIANS_TO_DEGREES
    call this%formatter%put_var('longitude',x,_RC)
    x(:) = this%lats(:) * MAPL_RADIANS_TO_DEGREES
    call this%formatter%put_var('latitude',x,_RC)
!    call this%formatter%put_var('mask_id',this%mask_id,_RC)
!    call this%formatter%put_var('mask_name',this%mask_name,_RC)

    _RETURN(_SUCCESS)
  end subroutine create_file_handle


   module subroutine close_file_handle(this,rc)
    class(MaskSamplerGeosat), intent(inout) :: this
    integer, optional, intent(out)          :: rc

    integer :: status
    if (trim(this%ofile) /= '') then
       if (mapl_am_i_root()) then
          call this%formatter%close(_RC)
       end if
    end if
    _RETURN(_SUCCESS)
  end subroutine close_file_handle


  module function compute_time_for_current(this,current_time,rc) result(rtime)
    use  MAPL_NetCDF, only : convert_NetCDF_DateTime_to_ESMF
    class(MaskSamplerGeosat), intent(inout) :: this
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



end submodule MaskSamplerGeosat_implement

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
     type(LocStreamFactory) :: LSF
     type(ESMF_LocStream) :: esmf_ls
     type(LocstreamRegridder) :: regridder
     integer :: nstation
     integer, allocatable :: station_id(:)
     character(len=ESMF_MAXSTR), allocatable :: station_name(:)
     real*8,  allocatable :: longitude(:)
     real*8,  allocatable :: latitude(:)
     real*8,  allocatable :: elevation(:)
     !
     type(ESMF_FieldBundle) :: bundle
     type(ESMF_FieldBundle) :: output_bundle
     type(GriddedIOitemVector) :: items
     type(FileMetadata) :: metadata
     type(VerticalData) :: vdata
     type(NetCDF4_FileFormatter) :: file_handle
     logical :: do_vertical_regrid
     integer :: number_written      !  what is this ??
     character(LEN=ESMF_MAXPATHLEN) :: file_name
   contains
     procedure :: add_metadata_route_handle
     procedure :: create_variable
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: create_output_bundle
     procedure :: append_file
  end type StationSampler

  interface StationSampler
     module procedure new_StationSampler_readfile
  end interface StationSampler

  
contains

  function new_StationSampler_readfile (filename,rc) result(sampler)
    type(StationSampler) :: sampler
    character(len=ESMF_MAXSTR), intent(in) :: filename
    integer, optional, intent(out) :: rc
    ! loc
    integer :: ios
    character(len=ESMF_MAXSTR) :: mystring
    character(len=ESMF_MAXSTR), allocatable :: string_pieces(:)
    integer :: max_len, max_seg, nseg
    integer :: i, iunit, nstation, status
    !
    !
    !_ 1. read from file
    !     read_lon_lat_elevate(filename): text file
    max_len=ESMF_MAXSTR
    max_seg=100       ! segmane separated by ',' on each line
    allocate(string_pieces(max_seg))

    iunit=11
    do while (iunit < 5000)
!! ??
!!       inquire(iunit, status=istat)
!!       if (istat /= 0) iunit=iunit+1
    enddo
    !
    open (iunit, file=trim(filename), status='unknown')
    i=0
    do while (ios==0)
       read (iunit, *, IOSTAT=ios, ERR=101)  mystring
       i=i+1
    enddo
101 continue
    nstation=i-2   ! minus 2 lines from header
    allocate(sampler%longitude(nstation))
    allocate(sampler%latitude(nstation))
    allocate(sampler%elevation(nstation))
    rewind(iunit)  !!; read(iunit, ('//'))
    read(iunit, *)
    read(iunit, *)
    do i=1, nstation
       sampler%station_id(i)=i
       read(iunit,*) mystring
       call split_string (mystring, ',', max_len, max_seg, nseg, string_pieces, status)
       read(string_pieces(1),*) sampler%station_name(i)
       read(string_pieces(2),*) sampler%longitude(i)
       read(string_pieces(3),*) sampler%latitude(i)
       read(string_pieces(4),*) sampler%elevation(i)
    enddo
        
    !_ 2. create LocStreamFactory with StationSampler: 
    !
    sampler%LSF = LocStreamFactory(sampler%longitude, sampler%latitude, _RC)
    sampler%esmf_ls = sampler%LSF%create_locstream(_RC)
    !
  end function new_StationSampler_readfile


  subroutine add_metadata_route_handle (this,items,bundle,vdata,rc)
    class(StationSampler), intent(inout) :: this
    type(GriddedIOitemVector), target, intent(inout) :: items
    type(ESMF_FieldBundle), intent(inout) :: bundle
    type(VerticalData), optional, intent(inout) :: vdata
    integer, optional, intent(out) :: rc   
!initialize(list(n)%items,list(n)%bundle,list(n)%timeInfo,vdata=list(n)%vdata,recycle_track=list(n)%recycle_track,_RC)
    
    integer :: status
    type(ESMF_Grid) :: grid
    !!type(ESMF_Clock) :: clock
    type(variable) :: v
    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item

    this%bundle=bundle
    this%items=items

    if (present(vdata)) then
       this%vdata=vdata
    else
       this%vdata=VerticalData(_RC)
    end if
    call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC)
    this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
    if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

!    v = variable(type=PFIO_REAL64,dimensions="time")
!    call v%add_attribute('units','degrees_east')
!    call v%add_attribute('long_name','longitude')
!    call this%metadata%add_variable(trim('longitude'),v)
!    v = variable(type=PFIO_REAL64,dimensions="time")
!    call v%add_attribute('units','degrees_east')
!    call v%add_attribute('long_name','latitude')
!    call this%metadata%add_variable(trim('latitude'),v)
    
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
    
    !  ??  this gird ==  history bundle grid ?? 
    !  ??  should it be  model grid          ??
    !
    call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
    this%regridder = LocStreamRegridder(grid,this%esmf_ls,_RC)
    call this%create_output_bundle(_RC)

  end subroutine add_metadata_route_handle
    

  
  subroutine create_output_bundle(this,rc)
    class(StationSampler), intent(inout) :: this
    integer, optional, intent(out) :: rc
    !
    integer :: status
    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    type(ESMF_Field) :: src_field,dst_field
    integer :: rank,lb(1),ub(1)
    !
    this%output_bundle = ESMF_FieldBundleCreate(_RC)
    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then
          call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
          call ESMF_FieldGet(src_field,rank=rank,_RC)
          if (rank==2) then
             dst_field = ESMF_FieldCreate(this%esmf_ls,name=trim(item%xname), &
                  typekind=ESMF_TYPEKIND_R4,_RC)
          else if (rank==3) then
             call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
             if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
                lb(1)=1
                ub(1)=this%vdata%lm
             end if             
             dst_field = ESMF_FieldCreate(this%esmf_ls,name=trim(item%xname), &
                  typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
          end if
          call MAPL_FieldBundleAdd(this%output_bundle,dst_field,_RC)
       else if (item%itemType == ItemTypeVector) then
          _FAIL("ItemTypeVector not yet supported")
       end if
       call iter%next()
    enddo    
  end subroutine create_output_bundle



  subroutine create_variable(this,vname,rc)
     class(StationSampler), intent(inout) :: this
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
        vdims = "time"
     else if (field_rank==3) then
        vdims = "time,lev"
     end if
     v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
     call v%add_attribute('units',trim(units))
     call v%add_attribute('long_name',trim(long_name))
     call v%add_attribute('missing_value',MAPL_UNDEF)
     call v%add_attribute('_FillValue',MAPL_UNDEF)
     call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
     call this%metadata%add_variable(trim(var_name),v,_RC)

  end subroutine create_variable


  
  subroutine append_file(this,current_time,rc)
     class(StationSampler), intent(inout) :: this
     type(ESMF_Time), intent(inout) :: current_time
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
     real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)
     !
     this%number_written=0
     if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
        call this%vdata%setup_eta_to_pressure(_RC)
     end if
     if (mapl_am_i_root()) then
        call this%file_handle%put_var('time',rtimes,&
             start=[this%number_written+1],count=[number_to_write],_RC)
        call this%file_handle%put_var('longitude',this%longitude(interval(1):interval(2)),&
             start=[this%number_written+1],count=[number_to_write],_RC)
        call this%file_handle%put_var('latitude',this%latitude(interval(1):interval(2)),&
             start=[this%number_written+1],count=[number_to_write],_RC)
     end if
     !
     iter = this%items%begin()
     do while (iter /= this%items%end())
        item => iter%get()
        if (item%itemType == ItemTypeScalar) then
           call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
           call ESMF_FieldBundleGet(this%output_bundle,trim(item%xname),field=dst_field,_RC)
           call ESMF_FieldGet(src_field,rank=rank,_RC)
           if (rank==2) then
              call ESMF_FieldGet(src_field,farrayptr=p_src_2d,_RC)
              call ESMF_FieldGet(dst_field,farrayptr=p_dst_2d,_RC)
              call this%regridder%regrid(p_src_2d,p_dst_2d,_RC)
              if (mapl_am_i_root()) then
                 call this%file_handle%put_var(trim(item%xname),p_dst_2d(interval(1):interval(2)),&
                      start=[this%number_written+1],count=[number_to_write])
              end if
           else if (rank==3) then
              call ESMF_FieldGet(src_field,farrayptr=p_src_3d,_RC)
              call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,_RC)
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 allocate(p_new_lev(size(p_src_3d,1),size(p_src_3d,2),this%vdata%lm),_STAT)
                 call this%vdata%regrid_eta_to_pressure(p_src_3d,p_new_lev,_RC)
                 call this%regridder%regrid(p_new_lev,p_dst_3d,_RC)
              else
                 call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)
              end if
              if (mapl_am_i_root()) then
                 call this%file_handle%put_var(trim(item%xname),p_dst_3d(interval(1):interval(2),:),&
                      start=[this%number_written+1,1],count=[number_to_write,size(p_dst_3d,2)])
              end if
           end if
        else if (item%itemType == ItemTypeVector) then
           _FAIL("ItemTypeVector not yet supported")
        end if
        call iter%next()
     enddo
     this%number_written=this%number_written+number_to_write
     !
  end subroutine append_file


  subroutine create_file_handle(this,filename,rc)
    class(StationSampler), intent(inout) :: this
    character(len=*), intent(inout) :: filename
    integer, optional, intent(out) :: rc
    !
    type(variable) :: v
    integer :: status
    
    this%file_name = trim(filename)
!    v = this%time_info%define_time_variable(_RC)
!    call this%metadata%modify_variable('time',v,_RC)
    if (mapl_am_I_root()) then
       call this%file_handle%create(trim(filename),_RC)
       call this%file_handle%write(this%metadata,_RC)
    end if
    this%number_written = 0
  end subroutine create_file_handle

  
  subroutine close_file_handle(this,rc)
    class(StationSampler), intent(inout) :: this
    integer, optional, intent(out) :: rc
    integer :: status
    !
    if (trim(this%file_name) /= '') then
       if (mapl_am_i_root()) then
          call this%file_handle%close(_RC)
       end if
    end if
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
    !
    !       mark
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
    str_piece(nseg+1:mxseg)='xxx'
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
!

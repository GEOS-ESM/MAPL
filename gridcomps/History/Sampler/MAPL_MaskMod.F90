#include "MAPL_Generic.h"

module MaskSamplerMod
  use ESMF
  use MAPL_ErrorHandlingMod
  use MAPL_KeywordEnforcerMod
  use LocStreamFactoryMod
  use MAPL_LocstreamRegridderMod
  use MAPL_FileMetadataUtilsMod
  use pFIO
  use MAPL_GriddedIOItemMod
  use MAPL_GriddedIOItemVectorMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_SortMod
  use MAPL_NetCDF
  use MAPL_StringTemplate
  use gFTL_StringVector
  use gFTL_StringStringMap
  use Plain_netCDF_Time
  use MAPL_ObsUtilMod
  use MPI
  use pFIO_FileMetadataMod, only : FileMetadata
  use pFIO_NetCDF4_FileFormatterMod, only : NetCDF4_FileFormatter
  use MAPL_GenericMod, only : MAPL_MetaComp, MAPL_TimerOn, MAPL_TimerOff
  use MPI, only  :  MPI_INTEGER, MPI_REAL, MPI_REAL8
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use pflogger, only: Logger, logging
  implicit none
  intrinsic :: size

  private

  public :: var2d_unit
  public :: var3d_unit
  type :: var2d_unit
     real(kind=REAL32), allocatable :: array_x(:)
  end type var2d_unit

  type :: var3d_unit
     real(kind=REAL32), allocatable :: array_xz(:,:)
     real(kind=REAL32), allocatable :: array_zx(:,:)
  end type var3d_unit


  public :: MaskSampler
  type :: MaskSampler
     character(len=ESMF_MAXSTR) :: grid_file_name
     !     we need on each PET
     !     npt_mask, index_mask(1:2,npt_mask)=[i,j]
     !
     integer :: npt_mask
     integer :: npt_mask_tot
     integer :: i1, in
     integer, allocatable :: index_mask(:,:)
     type(ESMF_FieldBundle) :: bundle
     type(GriddedIOitemVector) :: items
     type(VerticalData) :: vdata
     type(var2d_unit), allocatable :: var2d(:)
     type(var3d_unit), allocatable :: var3d(:)
     logical :: do_vertical_regrid
     type(TimeData)           :: timeinfo
     type(ESMF_Clock)         :: clock
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: epoch_frequency
     type(FileMetadata), allocatable, public:: metadata
     type(NetCDF4_FileFormatter) :: formatter
     character(len=ESMF_MAXSTR)  :: ofile
     integer :: write_collection_id
     logical :: use_pfio
     logical :: write_lev_first
     !
     integer                        :: nobs
     integer                        :: obs_written
     character(len=ESMF_MAXSTR)     :: index_name_x
     character(len=ESMF_MAXSTR)     :: index_name_y
     character(len=ESMF_MAXSTR)     :: index_name_location
     character(len=ESMF_MAXSTR)     :: index_name_lon
     character(len=ESMF_MAXSTR)     :: index_name_lat
     character(len=ESMF_MAXSTR)     :: index_name_loc
     character(len=ESMF_MAXSTR)     :: var_name_time
     character(len=ESMF_MAXSTR)     :: var_name_lat
     character(len=ESMF_MAXSTR)     :: var_name_lon
     character(len=ESMF_MAXSTR)     :: var_name_x
     character(len=ESMF_MAXSTR)     :: var_name_y
     character(len=ESMF_MAXSTR)     :: var_name_proj
     character(len=ESMF_MAXSTR)     :: att_name_proj

     integer :: xdim_true
     integer :: ydim_true
     integer :: thin_factor

     integer                        :: epoch        ! unit: second
     integer(kind=ESMF_KIND_I8)     :: epoch_index(2)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL32), allocatable :: lons_deg(:)
     real(kind=REAL32), allocatable :: lats_deg(:)

     real(kind=REAL32) :: rtime
     integer, allocatable :: recvcounts(:)
     integer, allocatable :: displs(:)
     type(MAPL_MetaComp), pointer :: GENSTATE

     integer, allocatable :: local_start(:)
     integer, allocatable :: global_start(:)
     integer, allocatable :: global_count(:)

     real, allocatable :: array_scalar_1d(:)
     real, allocatable :: array_scalar_2d(:,:)
     real, allocatable :: array_scalar_3d(:,:,:)
     logical :: itemOrderAlphabetical = .true.

     integer :: tmax     ! duration / freq

     real(kind=ESMF_KIND_R8), pointer:: obsTime(:)
     real(kind=ESMF_KIND_R8), allocatable:: t_alongtrack(:)
     integer                        :: nobs_dur
     integer                        :: nobs_dur_sum
     type(ESMF_Time)                :: obsfile_ref_time
     type(ESMF_TimeInterval)        :: obsfile_interval
     integer                        :: obsfile_Ts_index     ! for epoch
     integer                        :: obsfile_Te_index
     logical                        :: is_valid
   contains

     procedure :: initialize => initialize_
     procedure :: finalize => finalize_
     procedure :: create_metadata
     procedure :: regrid_append_file
     procedure :: create_Geosat_grid_find_mask
     procedure :: compute_time_for_current
     procedure :: set_param
     procedure :: stage2dlatlon
     procedure :: modifytime
     procedure :: alphabatize_variables
  end type MaskSampler

  interface MaskSampler
     module procedure MaskSampler_from_config
  end interface MaskSampler

  interface
     module function MaskSampler_from_config(config,string,clock,GENSTATE,rc) result(mask)
       use BinIOMod
       use pflogger, only         :  Logger, logging
       type(MaskSampler) :: mask
       type(ESMF_Config), intent(inout)        :: config
       character(len=*),  intent(in)           :: string
       type(ESMF_Clock),  intent(in)           :: clock
       type(MAPL_MetaComp), pointer, intent(in), optional  :: GENSTATE
       integer, optional, intent(out)          :: rc
     end function MaskSampler_from_config

     module subroutine initialize_(this,duration,frequency,items,bundle,timeInfo,vdata,global_attributes,reinitialize,rc)
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
     end subroutine initialize_

     module subroutine finalize_(this,rc)
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine finalize_

     module subroutine create_Geosat_grid_find_mask(this, rc)
       use pflogger, only: Logger, logging
       implicit none
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine create_Geosat_grid_find_mask

     module subroutine regrid_append_file (this,current_time,filename,oClients,rc)
       class(MaskSampler), intent(inout)       :: this
       type(ESMF_Time), intent(inout)          :: current_time
       character(len=*), intent(in)            :: filename
       type (ClientManager), target, optional, intent(inout) :: oClients
       integer, optional, intent(out)          :: rc
     end subroutine regrid_append_file

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
     end subroutine set_param

     module subroutine stage2dlatlon(this,filename,oClients,rc)
       class(MaskSampler), intent(inout) :: this
       character(len=*), intent(in) :: fileName
       type (ClientManager), optional, target, intent(inout) :: oClients
       integer, optional, intent(out) :: rc
     end subroutine stage2dlatlon

     module function compute_time_for_current(this,current_time,rc) result(rtime)
       use  MAPL_NetCDF, only : convert_NetCDF_DateTime_to_ESMF
       class(MaskSampler), intent(inout) :: this
       type(ESMF_Time), intent(in) :: current_time
       integer, optional, intent(out) :: rc
       real(kind=ESMF_KIND_R8) :: rtime
     end function compute_time_for_current

    module subroutine modifyTime(this, oClients, rc)
      class(MaskSampler), intent(inout) :: this
      type (ClientManager), optional, intent(inout) :: oClients
      integer, optional, intent(out) :: rc
    end subroutine modifyTime


  end interface

  contains

    ! These subroutines are not in the submodule due to an
    ! odd interaction with the NVHPC nvfortran compiler

    subroutine alphabatize_variables(this,nfixedVars,rc)
      class (MaskSampler), intent(inout) :: this
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

     subroutine  create_metadata(this,global_attributes,rc)
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
    type(StringVector) :: order
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
          if (this%write_lev_first) then
             vdims = "lev,mask_index"
          else
             vdims = "mask_index,lev"
          end if
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

end module MaskSamplerMod

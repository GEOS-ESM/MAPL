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

    ! WARNING: STUBBED OUT (tag v2.69.1+ifort2021.6 only; see CHANGELOG.md).
    ! The original bodies of alphabatize_variables and create_metadata trigger an
    ! internal compiler error (segfault) in ifort 2021.6.0, even at -O0. The same
    ! source compiles with ifort 2021.13.0. These are no-ops that return rc = -1
    ! and print an error if called, so 'sampler_type: mask' History collections
    ! will fail. Use a standard MAPL release with ifort 2021.13 or newer instead.

    subroutine alphabatize_variables(this,nfixedVars,rc)
      class (MaskSampler), intent(inout) :: this
      integer, intent(in) :: nFixedVars
      integer, optional, intent(out) :: rc

      if (present(rc)) rc = -1
      print *, 'ERROR: MaskSampler%alphabatize_variables is stubbed out in this build'
    end subroutine alphabatize_variables

    subroutine create_metadata(this,global_attributes,rc)
      class(MaskSampler), intent(inout) :: this
      type(StringStringMap), target, intent(in) :: global_attributes
      integer, optional, intent(out)          :: rc

      if (present(rc)) rc = -1
      print *, 'ERROR: MaskSampler%create_metadata is stubbed out in this build'
    end subroutine create_metadata

end module MaskSamplerMod

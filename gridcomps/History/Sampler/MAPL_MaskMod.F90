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

  private

  public :: MaskSampler
  type :: MaskSampler
     private
     !     character(len=:), allocatable :: grid_file_name
     character(len=ESMF_MAXSTR) :: grid_file_name
     !     we need on each PET
     !     npt_mask, index_mask(1:2,npt_mask)=[i,j]
     !
     integer :: npt_mask
     integer :: npt_mask_tot
     integer, allocatable :: index_mask(:,:)
     type(ESMF_FieldBundle) :: bundle
     type(GriddedIOitemVector) :: items
     type(VerticalData) :: vdata
     logical :: do_vertical_regrid
     type(TimeData)           :: timeinfo
     type(ESMF_Clock)         :: clock
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: epoch_frequency
     type(FileMetadata), allocatable, public:: metadata
     type(NetCDF4_FileFormatter) :: formatter
     character(len=ESMF_MAXSTR)  :: ofile
     integer :: write_collection_id
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
     integer, allocatable :: recvcounts(:)
     integer, allocatable :: displs(:)
     type(MAPL_MetaComp), pointer :: GENSTATE

     integer, allocatable :: local_start(:)
     integer, allocatable :: global_start(:)
     integer, allocatable :: global_count(:)
     
     real(kind=ESMF_KIND_R8), pointer:: obsTime(:)
     real(kind=ESMF_KIND_R8), allocatable:: t_alongtrack(:)
     integer                        :: nobs_dur
     integer                        :: nobs_dur_sum
     type(ESMF_Time)                :: obsfile_start_time   ! user specify
     type(ESMF_Time)                :: obsfile_end_time
     type(ESMF_TimeInterval)        :: obsfile_interval
     integer                        :: obsfile_Ts_index     ! for epoch
     integer                        :: obsfile_Te_index
     logical                        :: is_valid
   contains
     procedure :: initialize => initialize_
     procedure :: create_metadata
!!     procedure :: create_file_handle
     procedure :: append_file => output_to_server
     procedure :: create_grid => create_Geosat_grid_find_mask
     procedure :: compute_time_for_current
     procedure :: set_param
     procedure :: stage2dlatlon
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

     module subroutine initialize_(this,items,bundle,timeInfo,vdata,reinitialize,rc)
       class(MaskSampler), intent(inout) :: this
       type(GriddedIOitemVector), optional, intent(inout) :: items
       type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
       type(TimeData), optional, intent(inout)           :: timeInfo
       type(VerticalData), optional, intent(inout)       :: vdata
       logical, optional, intent(in)           :: reinitialize
       integer, optional, intent(out)          :: rc
     end subroutine initialize_

     module subroutine create_Geosat_grid_find_mask(this, rc)
       use pflogger, only: Logger, logging
       implicit none

       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine create_Geosat_grid_find_mask

!!     module subroutine set_param(this,deflation,quantize_algorithm,quantize_level,chunking,nbits_to_keep,regrid_method,itemOrder,write_collection_id,regrid_hints,rc)
!!        class (MaskSampler), intent(inout) :: this
!!        integer, optional, intent(in) :: deflation
!!        integer, optional, intent(in) :: quantize_algorithm
!!        integer, optional, intent(in) :: quantize_level
!!        integer, optional, intent(in) :: chunking(:)
!!        integer, optional, intent(in) :: nbits_to_keep
!!        integer, optional, intent(in) :: regrid_method
!!        logical, optional, intent(in) :: itemOrder
!!        integer, optional, intent(in) :: write_collection_id
!!        integer, optional, intent(in) :: regrid_hints
!!        integer, optional, intent(out) :: rc
!!     end subroutine set_param

     module subroutine  create_metadata(this,rc)
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine create_metadata

     module subroutine create_file_handle(this,filename,rc)
       class(MaskSampler), intent(inout) :: this
       character(len=*), intent(in)            :: filename
       integer, optional, intent(out)          :: rc
     end subroutine create_file_handle

     module subroutine close_file_handle(this,rc)
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine close_file_handle

     module subroutine output_to_server(this,current_time,filename,oClients,rc)
       class(MaskSampler), intent(inout)       :: this
       type(ESMF_Time), intent(inout)          :: current_time
       character(len=*), intent(in)            :: filename
       type (ClientManager), optional, intent(inout) :: oClients
       integer, optional, intent(out)          :: rc
     end subroutine output_to_server

     module subroutine append_file_oserver(this,current_time,rc)
       class(MaskSampler), intent(inout) :: this
       type(ESMF_Time), intent(inout)          :: current_time
       integer, optional, intent(out)          :: rc
     end subroutine append_file_oserver

     module subroutine set_param(this,deflation,quantize_algorithm,quantize_level,chunking,&
          nbits_to_keep,regrid_method,itemOrder,write_collection_id,regrid_hints,rc)
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

     
  end interface
end module MaskSamplerMod

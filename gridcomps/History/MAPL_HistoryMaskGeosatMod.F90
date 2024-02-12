module MaskSamplerGeosatMod
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
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use pflogger, only: Logger, logging
  implicit none

  private

  public :: MaskSamplerGeosat
  type :: MaskSamplerGeosat
     private
     !     character(len=:), allocatable :: grid_file_name
     character(len=ESMF_MAXSTR) :: grid_file_name
     !-- ygyu we donot need LS
     !
     !   we need on each PET
     !     npt_mask, index_mask(1:2,npt_mask)=[i,j]
     !
     integer :: npt_mask
     integer :: npt_mask_tot
     integer, allocatable :: index_mask(:,:)
     !
          type(ESMF_FieldBundle) :: bundle
          type(ESMF_FieldBundle) :: output_bundle
     !     type(ESMF_FieldBundle) :: acc_bundle
     !     type(ESMF_Field)       :: fieldA
     !     type(ESMF_Field)       :: fieldB

     type(GriddedIOitemVector) :: items
     type(VerticalData) :: vdata
     logical :: do_vertical_regrid
     character(len=ESMF_MAXSTR)     :: ofile
     type(TimeData)           :: time_info
     type(ESMF_Clock)         :: clock
     type(ESMF_Alarm), public :: alarm
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: epoch_frequency
     type(FileMetadata)       :: metadata
     type(NetCDF4_FileFormatter) :: formatter


     integer                        :: nobs_type
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
     procedure :: initialize
     procedure :: add_metadata
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: append_file =>  regrid_accumulate_append_file
!     procedure :: create_new_bundle
     procedure :: create_grid => create_Geosat_grid_find_mask
     procedure :: compute_time_for_current
  end type MaskSamplerGeosat

  interface MaskSamplerGeosat
     module procedure MaskSamplerGeosat_from_config
  end interface MaskSamplerGeosat


  interface
     module function MaskSamplerGeosat_from_config(config,string,clock,rc) result(mask)
       type(MaskSamplerGeosat) :: mask
       type(ESMF_Config), intent(inout)        :: config
       character(len=*),  intent(in)           :: string
       type(ESMF_Clock),  intent(in)           :: clock
       integer, optional, intent(out)          :: rc
     end function MaskSamplerGeosat_from_config

     module subroutine initialize(this,items,bundle,timeInfo,vdata,reinitialize,rc)
       class(MaskSamplerGeosat), intent(inout) :: this
       type(GriddedIOitemVector), optional, intent(inout) :: items
       type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
       type(TimeData), optional, intent(inout)           :: timeInfo
       type(VerticalData), optional, intent(inout)       :: vdata
       logical, optional, intent(in)           :: reinitialize
       integer, optional, intent(out)          :: rc
     end subroutine initialize

     module subroutine create_Geosat_grid_find_mask(this, rc)
       class(MaskSamplerGeosat), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine create_Geosat_grid_find_mask

!!     module function create_new_bundle(this,rc) result(new_bundle)
!!       class(MaskSamplerGeosat), intent(inout) :: this
!!       type(ESMF_FieldBundle)                  :: new_bundle
!!       integer, optional, intent(out)          :: rc
!!     end function create_new_bundle

     !!     module subroutine  add_metadata(this,currTime,rc)
     module subroutine  add_metadata(this,rc)
       class(MaskSamplerGeosat), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine add_metadata

     module subroutine create_file_handle(this,filename,rc)
       class(MaskSamplerGeosat), intent(inout) :: this
       character(len=*), intent(in)            :: filename
       integer, optional, intent(out)          :: rc
     end subroutine create_file_handle

     module subroutine close_file_handle(this,rc)
       class(MaskSamplerGeosat), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine close_file_handle

     module subroutine regrid_accumulate_append_file(this,current_time,rc)
       class(MaskSamplerGeosat), intent(inout) :: this
       type(ESMF_Time), intent(inout)          :: current_time
       integer, optional, intent(out)          :: rc
     end subroutine regrid_accumulate_append_file

     module function compute_time_for_current(this,current_time,rc) result(rtime)
       class(MaskSamplerGeosat), intent(inout) :: this
       type(ESMF_Time), intent(in) :: current_time
       integer, optional, intent(out) :: rc
       real(kind=ESMF_KIND_R8) :: rtime
     end function compute_time_for_current

  end interface
end module MaskSamplerGeosatMod

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
!!  use BinIOMod, only : GETFILE
  use MAPL_CommsMod
  use MAPL_SortMod
  use MAPL_NetCDF
  use MAPL_StringTemplate
  use Plain_netCDF_Time
  use MAPL_ObsUtilMod
  use pFIO_FileMetadataMod, only : FileMetadata
  use pFIO_NetCDF4_FileFormatterMod, only : NetCDF4_FileFormatter
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use pflogger, only: Logger, logging
  implicit none

  integer, parameter :: mx_file = 300
  private

  public :: MaskSampler
  type :: MaskSampler
     private
     type(ESMF_LocStream)   :: LS_rt        !  obs LS on root
     type(ESMF_LocStream)   :: LS_ds        !  distributed
     !!type(ESMF_LocStream)   :: mask_LS_rt   !  output
     !!type(ESMF_LocStream)   :: mask_LS_ds
     type(LocStreamFactory) :: locstream_factory
     type(obs_unit),    allocatable :: obs(:)
     type(ESMF_Time),   allocatable :: times(:)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)

     real(kind=REAL64), allocatable :: lons_ds(:)
     real(kind=REAL64), allocatable :: lats_ds(:)     

!     real(kind=REAL64), allocatable :: lons_ds(:)
!     real(kind=REAL64), allocatable :: lats_ds(:)     
     real(kind=REAL64), allocatable :: times_R8(:)
     integer,           allocatable :: obstype_id(:)

     ! in case obs is swath type, we convert 2d to 1d vector
     real(kind=REAL64), allocatable :: lons_2d(:,:)
     real(kind=REAL64), allocatable :: lats_2d(:,:)
     real(kind=REAL64), allocatable :: times_R8_2d(:,:)

     integer, allocatable :: mask(:,:)

     type(ESMF_FieldBundle) :: bundle
     type(ESMF_FieldBundle) :: output_bundle
     type(ESMF_FieldBundle) :: acc_bundle
     type(ESMF_Field)       :: fieldA
     type(ESMF_Field)       :: fieldB

     type(GriddedIOitemVector) :: items
     type(VerticalData) :: vdata
     logical :: do_vertical_regrid

     type(LocstreamRegridder) :: regridder
     type(TimeData)           :: time_info
     type(ESMF_Clock)         :: clock
     type(ESMF_Alarm), public :: alarm
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: epoch_frequency
     type(FileMetadata), allocatable :: metadata
     type(NetCDF4_FileFormatter), allocatable :: file_handle

     integer                        :: nobs_type
     character(len=ESMF_MAXSTR)     :: index_name_x
     character(len=ESMF_MAXSTR)     :: index_name_location
     character(len=ESMF_MAXSTR)     :: index_name_lon
     character(len=ESMF_MAXSTR)     :: index_name_lat
     character(len=ESMF_MAXSTR)     :: index_name_loc
     character(len=ESMF_MAXSTR)     :: var_name_time
     character(len=ESMF_MAXSTR)     :: var_name_lat
     character(len=ESMF_MAXSTR)     :: var_name_lon
     character(len=ESMF_MAXSTR)     :: datetime_units
     character(len=ESMF_MAXSTR)     :: tunit
     character(len=ESMF_MAXSTR)     :: input_template
     character(len=ESMF_MAXSTR)     :: filenames(mx_file)
     integer                        :: M_file

     integer                        :: epoch        ! unit: second
     integer(kind=ESMF_KIND_I8)     :: epoch_index(2)
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
     procedure :: create_variable => create_metadata_variable
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: append_file
     procedure :: create_new_bundle
     procedure :: create_grid
     procedure :: find_mask
     procedure :: destroy_rh_regen_LS
     procedure :: get_x_subset
  end type MaskSampler

  interface MaskSampler
     module procedure MaskSampler_from_config
  end interface MaskSampler


  interface
     module function MaskSampler_from_config(config,string,clock,rc) result(mask)
       type(MaskSampler) :: mask
       type(ESMF_Config), intent(inout)        :: config
       character(len=*),  intent(in)           :: string
       type(ESMF_Clock),  intent(in)           :: clock
       integer, optional, intent(out)          :: rc
     end function MaskSampler_from_config

     module subroutine initialize(this,items,bundle,timeInfo,vdata,reinitialize,rc)
       class(MaskSampler), intent(inout) :: this
       type(GriddedIOitemVector), optional, intent(inout) :: items
       type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
       type(TimeData), optional, intent(inout)           :: timeInfo
       type(VerticalData), optional, intent(inout) :: vdata
       logical, optional, intent(in)           :: reinitialize
       integer, optional, intent(out)          :: rc
     end subroutine initialize

     module subroutine  create_metadata_variable(this,vname,rc)
       class(MaskSampler), intent(inout) :: this
       character(len=*), intent(in)            :: vname
       integer, optional, intent(out)          :: rc
     end subroutine create_metadata_variable

     module function create_new_bundle(this,rc) result(new_bundle)
       class(MaskSampler), intent(inout) :: this
       type(ESMF_FieldBundle)                  :: new_bundle
       integer, optional, intent(out)          :: rc
     end function create_new_bundle

     module subroutine create_file_handle(this,filename_suffix,rc)
       class(MaskSampler), intent(inout) :: this
       character(len=*), intent(in)            :: filename_suffix
       integer, optional, intent(out)          :: rc
     end subroutine create_file_handle

     module subroutine close_file_handle(this,rc)
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine close_file_handle

     module subroutine append_file(this,current_time,rc)
       class(MaskSampler), intent(inout) :: this
       type(ESMF_Time), intent(inout)          :: current_time
       integer, optional, intent(out)          :: rc
     end subroutine append_file

     module subroutine create_grid(this, rc)
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine create_grid

     module subroutine find_mask (this, rc)
       implicit none
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine find_mask

     module subroutine regrid_accumulate_on_xsubset (this, rc)
       implicit none
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine regrid_accumulate_on_xsubset

     module subroutine get_x_subset(this, interval, x_subset, rc)
       class(MaskSampler), intent(inout) :: this
       type(ESMF_Time), intent(in)             :: interval(2)
       integer, intent(out)                    :: x_subset(2)
       integer, optional, intent(out)          :: rc
     end subroutine get_x_subset

     module subroutine destroy_rh_regen_LS (this, rc)
       class(MaskSampler), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine destroy_rh_regen_LS

  end interface
end module MaskSamplerMod

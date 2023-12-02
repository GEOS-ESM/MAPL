module HistoryMaskMod
  use ESMF
  use MAPL_FileMetadataUtilsMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use LocStreamFactoryMod
  use MAPL_LocstreamRegridderMod
  use MAPL_ObsUtilMod
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none

  private

  public :: HistoryMask
  type :: HistoryMask
     private
     type(ESMF_LocStream)   :: obs_LS_rt    !  aux
     type(ESMF_LocStream)   :: obs_LS_ds
     type(ESMF_LocStream)   :: mask_LS_rt   !  output
     type(ESMF_LocStream)   :: mask_LS_ds 
     type(LocStreamFactory) :: locstream_factory
     type(obs_unit),    allocatable :: obs(:)
     type(ESMF_Time),   allocatable :: times(:)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL64), allocatable :: times_R8(:)
     integer,           allocatable :: obstype_id(:)

     ! in case obs is swath type, we convert 2d to 1d vector
     real(kind=REAL64), allocatable :: lons_2d(:,:)
     real(kind=REAL64), allocatable :: lats_2d(:,:)
     real(kind=REAL64), allocatable :: times_R8_2d(:,:)
     
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

     integer                        :: nobs_type
!     character(len=ESMF_MAXSTR)     :: nc_index
!     character(len=ESMF_MAXSTR)     :: nc_time
!     character(len=ESMF_MAXSTR)     :: nc_latitude
!     character(len=ESMF_MAXSTR)     :: nc_longitude

     character(len=ESMF_MAXSTR)     :: index_name_location     
     character(len=ESMF_MAXSTR)     :: index_name_lon
     character(len=ESMF_MAXSTR)     :: index_name_lat     
     character(len=ESMF_MAXSTR)     :: var_name_time
     character(len=ESMF_MAXSTR)     :: var_name_lat
     character(len=ESMF_MAXSTR)     :: var_name_lon
     character(len=ESMF_MAXSTR)     :: datetime_units
     integer                        :: epoch        ! unit: second
     integer(kind=ESMF_KIND_I8)     :: epoch_index(2)
     real(kind=ESMF_KIND_R8), pointer:: obsTime(:)
     integer                        :: nobs_epoch
     integer                        :: nobs_epoch_sum
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
     procedure :: regrid_accumulate => regrid_accumulate_on_xsubset
     procedure :: destroy_rh_regen_LS
     procedure :: get_x_subset
  end type HistoryMask

  interface HistoryMask
     module procedure HistoryMask_from_config
  end interface HistoryMask


  interface
     module function HistoryMask_from_config(config,string,clock,rc) result(mask)
       type(HistoryMask) :: mask
       type(ESMF_Config), intent(inout)        :: config
       character(len=*),  intent(in)           :: string
       type(ESMF_Clock),  intent(in)           :: clock
       integer, optional, intent(out)          :: rc
     end function HistoryMask_from_config

     module subroutine initialize(this,items,bundle,timeInfo,vdata,reinitialize,rc)
       class(HistoryMask), intent(inout) :: this
       type(GriddedIOitemVector), optional, intent(inout) :: items
       type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
       type(TimeData), optional, intent(inout)           :: timeInfo
       type(VerticalData), optional, intent(inout) :: vdata
       logical, optional, intent(in)           :: reinitialize
       integer, optional, intent(out)          :: rc
     end subroutine initialize

     module subroutine  create_metadata_variable(this,vname,rc)
       class(HistoryMask), intent(inout) :: this
       character(len=*), intent(in)            :: vname
       integer, optional, intent(out)          :: rc
     end subroutine create_metadata_variable

     module function create_new_bundle(this,rc) result(new_bundle)
       class(HistoryMask), intent(inout) :: this
       type(ESMF_FieldBundle)                  :: new_bundle
       integer, optional, intent(out)          :: rc
     end function create_new_bundle

     module subroutine create_file_handle(this,filename_suffix,rc)
       class(HistoryMask), intent(inout) :: this
       character(len=*), intent(in)            :: filename_suffix
       integer, optional, intent(out)          :: rc
     end subroutine create_file_handle

     module subroutine close_file_handle(this,rc)
       class(HistoryMask), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine close_file_handle

     module subroutine append_file(this,current_time,rc)
       class(HistoryMask), intent(inout) :: this
       type(ESMF_Time), intent(inout)          :: current_time
       integer, optional, intent(out)          :: rc
     end subroutine append_file

     module subroutine create_grid(this, rc)
       class(HistoryMask), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine create_grid

     module subroutine regrid_accumulate_on_xsubset (this, rc)
       implicit none
       class(HistoryMask), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine regrid_accumulate_on_xsubset

     module subroutine get_x_subset(this, interval, x_subset, rc)
       class(HistoryMask), intent(inout) :: this
       type(ESMF_Time), intent(in)             :: interval(2)
       integer, intent(out)                    :: x_subset(2)
       integer, optional, intent(out)          :: rc
     end subroutine get_x_subset

     module subroutine destroy_rh_regen_LS (this, rc)
       class(HistoryMask), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine destroy_rh_regen_LS

  end interface
end module HistoryMaskMod

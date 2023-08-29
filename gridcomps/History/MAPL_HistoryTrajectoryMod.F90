module HistoryTrajectoryMod
  use ESMF
  use MAPL_FileMetadataUtilsMod
  use pfio_FileMetadataMod
  use pfio_NetCDF4_FileFormatterMod
  use pfio_VariableMod
  use MAPL_GriddedIOItemVectorMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use LocStreamFactoryMod
  use MAPL_LocstreamRegridderMod
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  private

  public :: HistoryTrajectory

  type :: HistoryTrajectory
     private
     type(ESMF_LocStream) :: LS_rt  !  root
     type(ESMF_LocStream) :: LS_ds  !  distribute
     type(LocStreamFactory) :: locstream_factory
     type(ESMF_Time), allocatable :: times(:)
     real(kind=REAL64), allocatable :: times_R8(:)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     type(ESMF_FieldBundle) :: bundle
     type(ESMF_FieldBundle) :: output_bundle
     type(ESMF_FieldBundle) :: acc_bundle
     type(ESMF_Field)       :: fieldA
     type(ESMF_Field)       :: fieldB

     type(GriddedIOitemVector) :: items
     type(FileMetadata) :: metadata
     type(VerticalData) :: vdata
     logical :: do_vertical_regrid
     type(NetCDF4_FileFormatter) :: file_handle
     type(LocstreamRegridder) :: regridder
     character(LEN=ESMF_MAXPATHLEN) :: file_name
     type(TimeData) :: time_info
     logical :: recycle_track
     type(ESMF_Clock)         :: clock
     type(ESMF_Alarm), public :: alarm
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: epoch_frequency

     character(len=ESMF_MAXSTR)     :: obsFile
     character(len=ESMF_MAXSTR)     :: obsfile_template
     character(len=ESMF_MAXSTR)     :: nc_index
     character(len=ESMF_MAXSTR)     :: nc_time
     character(len=ESMF_MAXSTR)     :: nc_latitude
     character(len=ESMF_MAXSTR)     :: nc_longitude
     character(len=ESMF_MAXSTR)     :: var_name_time
     character(len=ESMF_MAXSTR)     :: var_name_lat
     character(len=ESMF_MAXSTR)     :: var_name_lon
     character(len=ESMF_MAXSTR)     :: datetime_units
     integer                        :: epoch        ! unit: second
     integer(kind=ESMF_KIND_I8)     :: epoch_index(2)
     real(kind=ESMF_KIND_R8), pointer:: obsTime(:)
     integer :: nobs_epoch
     integer :: nobs_epoch_sum
     type(ESMF_Time)                :: obsfile_start_time   ! user specify
     type(ESMF_Time)                :: obsfile_end_time
     type(ESMF_TimeInterval)        :: obsfile_interval
     integer                        :: obsfile_Ts_index     ! for epoch
     integer                        :: obsfile_Te_index
     logical                        :: obsfile_is_available

   contains
     procedure :: initialize
     procedure :: create_variable => create_metadata_variable
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: append_file

     procedure :: create_new_bundle
     procedure :: reset_times_to_current_day
     procedure :: time_real_to_ESMF

     procedure :: create_grid
     procedure :: regrid_accumulate => regrid_accumulate_on_xsubset
     procedure :: destroy_rh_regen_LS
     procedure :: get_x_subset
     procedure :: get_obsfile_Tbracket_from_epoch
     procedure :: get_filename_from_template_use_index

  end type HistoryTrajectory

  interface HistoryTrajectory
     module procedure HistoryTrajectory_from_config
  end interface HistoryTrajectory

  interface
     module function HistoryTrajectory_from_config(config,string,clock,rc) result(traj)
       type(HistoryTrajectory) :: traj
       type(ESMF_Config), intent(inout)       :: config
       character(len=*),  intent(in)          :: string
       type(ESMF_Clock),  intent(in)          :: clock
       integer, optional, intent(out)         :: rc
     end function HistoryTrajectory_from_config

     module subroutine initialize(this,items,bundle,timeInfo,vdata,recycle_track,rc)
       class(HistoryTrajectory), intent(inout) :: this
       type(GriddedIOitemVector), target, intent(inout) :: items
       type(ESMF_FieldBundle), intent(inout)   :: bundle
       type(TimeData), intent(inout)           :: timeInfo
       type(VerticalData), optional, intent(inout) :: vdata
       logical, optional, intent(inout)        :: recycle_track
       integer, optional, intent(out)          :: rc
     end subroutine initialize

     module subroutine  create_metadata_variable(this,vname,rc)
       class(HistoryTrajectory), intent(inout) :: this
       character(len=*), intent(in)            :: vname
       integer, optional, intent(out)          :: rc
     end subroutine create_metadata_variable

     module function create_new_bundle(this,rc) result(new_bundle)
       class(HistoryTrajectory), intent(inout) :: this
       type(ESMF_FieldBundle)                  :: new_bundle
       integer, optional, intent(out)          :: rc
     end function create_new_bundle

     module subroutine create_file_handle(this,filename,rc)
       class(HistoryTrajectory), intent(inout) :: this
       character(len=*), intent(inout)         :: filename
       integer, optional, intent(out)          :: rc
     end subroutine create_file_handle

     module subroutine close_file_handle(this,rc)
       class(HistoryTrajectory), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine close_file_handle

     module subroutine append_file(this,current_time,rc)
       class(HistoryTrajectory), intent(inout) :: this
       type(ESMF_Time), intent(inout)          :: current_time
       integer, optional, intent(out)          :: rc
     end subroutine append_file

     module subroutine reset_times_to_current_day(this,rc)
       class(HistoryTrajectory), intent(Inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine reset_times_to_current_day

     module subroutine sort_three_arrays_by_time(U,V,T,rc)
       real(ESMF_KIND_R8) :: U(:), V(:), T(:)
       integer, optional, intent(out)          :: rc
     end subroutine sort_three_arrays_by_time

     module subroutine time_real_to_ESMF (this,rc)
       class(HistoryTrajectory), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine time_real_to_ESMF

     module subroutine create_grid(this, rc)
       class(HistoryTrajectory), intent(inout) :: this
       integer, optional, intent(out)          :: rc
    end subroutine create_grid

     module subroutine regrid_accumulate_on_xsubset (this, rc)
       implicit none
       class(HistoryTrajectory), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine regrid_accumulate_on_xsubset

     module subroutine get_x_subset(this, interval, x_subset, rc)
       class(HistoryTrajectory), intent(inout) :: this
       type(ESMF_Time), intent(in)             :: interval(2)
       integer, intent(out)                    :: x_subset(2)
       integer, optional, intent(out)          :: rc
     end subroutine get_x_subset

     module subroutine destroy_rh_regen_LS (this, rc)
       class(HistoryTrajectory), intent(inout) :: this
       integer, optional, intent(out)          :: rc
     end subroutine destroy_rh_regen_LS

     module subroutine get_obsfile_Tbracket_from_epoch(this, currTime, rc)
       class(HistoryTrajectory), intent(inout) :: this
       type(ESMF_Time), intent(in)             :: currTime
       integer, optional, intent(out)          :: rc
     end subroutine get_obsfile_Tbracket_from_epoch

     module function get_filename_from_template (time, file_template, rc) result(filename)
       type(ESMF_Time), intent(in)             :: time
       character(len=*), intent(in)            :: file_template
       character(len=ESMF_MAXSTR)              :: filename
       integer, optional, intent(out)          :: rc
     end function get_filename_from_template

     module function get_filename_from_template_use_index (this, f_index, rc) result(filename)
       class(HistoryTrajectory), intent(inout) :: this
!       character(len=*), intent(in)            :: file_template
       character(len=ESMF_MAXSTR)              :: filename
       integer, intent(in)                     :: f_index
       integer, optional, intent(out)          :: rc
     end function get_filename_from_template_use_index

  end interface

end module HistoryTrajectoryMod

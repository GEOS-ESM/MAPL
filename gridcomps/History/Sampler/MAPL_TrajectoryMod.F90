module HistoryTrajectoryMod
  use ESMF
  use MAPL_FileMetadataUtilsMod
  use MAPL_GriddedIOItemVectorMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use LocStreamFactoryMod
  use MAPL_LocstreamRegridderMod
  use MAPL_ObsUtilMod
  use MAPL_GenericMod, only : MAPL_MetaComp

  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none

  private

  public :: HistoryTrajectory
  type :: HistoryTrajectory
     private
     type(ESMF_LocStream)   :: LS_rt
     type(ESMF_LocStream)   :: LS_ds
     type(ESMF_LocStream)   :: LS_chunk
     type(LocStreamFactory) :: locstream_factory
     type(obs_unit),    allocatable :: obs(:)
     type(ESMF_Time),   allocatable :: times(:)
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     real(kind=REAL64), allocatable :: times_R8(:)
     integer,           allocatable :: obstype_id(:)
     integer,           allocatable :: location_index_ioda(:)   ! location index in its own ioda file
     type(MAPL_MetaComp), pointer   :: GENSTATE

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
     type(ESMF_TimeInterval), public  :: epoch_frequency

     integer                        :: nobs_type
     character(len=ESMF_MAXSTR)     :: index_name_x
     character(len=ESMF_MAXSTR)     :: var_name_time
     character(len=ESMF_MAXSTR)     :: var_name_lat
     character(len=ESMF_MAXSTR)     :: var_name_lon
     character(len=ESMF_MAXSTR)     :: var_name_time_full
     character(len=ESMF_MAXSTR)     :: var_name_lat_full
     character(len=ESMF_MAXSTR)     :: var_name_lon_full
     character(len=ESMF_MAXSTR)     :: datetime_units
     character(len=ESMF_MAXSTR)     :: Location_index_name
     logical                        :: use_NWP_1_file = .false.
     logical                        :: restore_2_obs_vector = .false.
     integer                        :: epoch        ! unit: second
     integer(kind=ESMF_KIND_I8)     :: epoch_index(2)
     real(kind=ESMF_KIND_R8), pointer:: obsTime(:)
     integer                        :: nobs_epoch
     integer                        :: nobs_epoch_sum
     type(ESMF_Time)                :: obsfile_start_time   ! user specify
     type(ESMF_TimeInterval)        :: obsfile_interval
     integer                        :: obsfile_Ts_index     ! for epoch
     integer                        :: obsfile_Te_index
     logical                        :: active               ! case: when no obs. exist
     logical                        :: level_by_level = .false.
     integer                        :: schema_version     
     !
     ! note
     ! for MPI_GATHERV of 3D data in procedure :: append_file
     ! we have choice LEVEL_BY_LEVEL or ALL_AT_ONCE  (timing in sec below for extdata)
     !    c1440_L137_M1260  57.276       69.870
     !    c5760_L137_M8820  98.494       93.140
     ! M=cores
     ! hence start using ALL_AT_ONCE from c5760+
   contains
     procedure :: initialize => initialize_
     procedure :: create_variable => create_metadata_variable
     procedure :: create_file_handle
     procedure :: close_file_handle
     procedure :: append_file
     procedure :: create_new_bundle
     procedure :: create_grid
     procedure :: regrid_accumulate => regrid_accumulate_on_xsubset
     procedure :: destroy_rh_regen_LS
     procedure :: get_x_subset
  end type HistoryTrajectory

  interface HistoryTrajectory
     module procedure HistoryTrajectory_from_config
  end interface HistoryTrajectory

  interface
     module function HistoryTrajectory_from_config(config,string,clock,schema_version,GENSTATE,rc) result(traj)
       type(HistoryTrajectory) :: traj
       type(ESMF_Config), intent(inout)        :: config
       character(len=*),  intent(in)           :: string
       type(ESMF_Clock),  intent(in)           :: clock
       integer, intent(in)                     :: schema_version
       type(MAPL_MetaComp), pointer, intent(in), optional  :: GENSTATE
       integer, optional, intent(out)          :: rc
     end function HistoryTrajectory_from_config

     module function HistoryTrajectory_from_config_schema_version_1 &
          (config,string,clock,schema_version,GENSTATE,rc) result(traj)
       type(HistoryTrajectory) :: traj
       type(ESMF_Config), intent(inout)        :: config
       character(len=*),  intent(in)           :: string
       type(ESMF_Clock),  intent(in)           :: clock
       integer, intent(in)                     :: schema_version       
       type(MAPL_MetaComp), pointer, intent(in), optional  :: GENSTATE
       integer, optional, intent(out)          :: rc
     end function HistoryTrajectory_from_config_schema_version_1

     module function HistoryTrajectory_from_config_schema_version_2 &
          (config,string,clock,schema_version,GENSTATE,rc) result(traj)
       type(HistoryTrajectory) :: traj
       type(ESMF_Config), intent(inout)        :: config
       character(len=*),  intent(in)           :: string
       type(ESMF_Clock),  intent(in)           :: clock
       integer, intent(in)                     :: schema_version
       type(MAPL_MetaComp), pointer, intent(in), optional  :: GENSTATE
       integer, optional, intent(out)          :: rc
     end function HistoryTrajectory_from_config_schema_version_2
     
     module subroutine initialize_(this,items,bundle,timeInfo,vdata,reinitialize,rc)
       class(HistoryTrajectory), intent(inout) :: this
       type(GriddedIOitemVector), optional, intent(inout) :: items
       type(ESMF_FieldBundle), optional, intent(inout)   :: bundle
       type(TimeData), optional, intent(inout)           :: timeInfo
       type(VerticalData), optional, intent(inout) :: vdata
       logical, optional, intent(in)           :: reinitialize
       integer, optional, intent(out)          :: rc
     end subroutine initialize_

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

     module subroutine create_file_handle(this,filename_suffix,rc)
       class(HistoryTrajectory), intent(inout) :: this
       character(len=*), intent(in)            :: filename_suffix
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

  end interface
end module HistoryTrajectoryMod

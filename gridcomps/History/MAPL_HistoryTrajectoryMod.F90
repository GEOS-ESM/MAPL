#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module HistoryTrajectoryMod
  
   use ESMF
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use MAPL_FileMetadataUtilsMod
   use LocStreamFactoryMod
   use pFIO
   use MAPL_GriddedIOItemVectorMod
   use MAPL_GriddedIOItemMod
   use MAPL_TimeDataMod
   use MAPL_VerticalDataMod
   use MAPL_BaseMod
   use MAPL_CommsMod
   use MAPL_SortMod
   use MAPL_NetCDF
   use MAPL_plain_netCDF_Time
   use MAPL_LocstreamRegridderMod
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: HistoryTrajectory

   type :: HistoryTrajectory
      private
      type(ESMF_LocStream) :: LS_rt
      type(ESMF_LocStream) :: LS_ds
      type(LocStreamFactory) :: locstream_factory
      type(ESMF_Time), allocatable :: times(:)
      real(kind=REAL64), allocatable :: times_R8(:)
      real(kind=REAL64), allocatable :: lons(:),lats(:)
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_FieldBundle) :: output_bundle
      type(ESMF_FieldBundle) :: acc_bundle      
      
      integer :: number_written
      type(GriddedIOitemVector) :: items
      type(FileMetadata) :: metadata
      type(VerticalData) :: vdata
      logical :: do_vertical_regrid
      type(NetCDF4_FileFormatter) :: file_handle
      type(LocstreamRegridder) :: regridder
      integer :: previous_index
      type(ESMF_Time) :: previous_time
      character(LEN=ESMF_MAXPATHLEN) :: file_name
      type(TimeData) :: time_info
      logical :: recycle_track
      type(ESMF_Clock)         :: clock
      type(ESMF_Alarm), public :: alarm
      type(ESMF_Time)          :: RingTime
      type(ESMF_TimeInterval)  :: Frequency_epoch

      character(len=ESMF_MAXSTR)     :: obsFile
      character(len=ESMF_MAXSTR)     :: nc_index
      character(len=ESMF_MAXSTR)     :: nc_time
      character(len=ESMF_MAXSTR)     :: nc_latitude
      character(len=ESMF_MAXSTR)     :: nc_longitude
      character(len=ESMF_MAXSTR)     :: var_name_time
      character(len=ESMF_MAXSTR)     :: var_name_lat
      character(len=ESMF_MAXSTR)     :: var_name_lon
      character(len=ESMF_MAXSTR)     :: datetime_units
      integer :: epoch                         ! unit: second
      integer(kind=ESMF_KIND_I8)     :: epoch_index(2)
      integer(ESMF_KIND_I4), pointer :: seqIndex(:)
      real(kind=ESMF_KIND_R8), pointer:: obsTime(:)
      integer :: nobs_epoch

    contains
!      procedure :: initialize
   end type HistoryTrajectory

   interface HistoryTrajectory
      module procedure HistoryTrajectory_from_config
   end interface HistoryTrajectory
   
   interface
      module function HistoryTrajectory_from_config(config,string,clock,unusable,rc) result(traj)
        type(HistoryTrajectory) :: traj
        type(ESMF_Config), intent(inout) :: config
        character(len=*),  intent(in)    :: string
        type(ESMF_Clock),  intent(in)    :: clock
        class (KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc
      end function HistoryTrajectory_from_config

      module subroutine initialize(this,items,bundle,timeInfo,unusable,vdata,recycle_track,rc)
        class(HistoryTrajectory), intent(inout) :: this
        type(GriddedIOitemVector), target, intent(inout) :: items
        type(ESMF_FieldBundle), intent(inout) :: bundle
        type(TimeData), intent(inout) :: timeInfo
        class (KeywordEnforcer), optional, intent(in) :: unusable
        type(VerticalData), optional, intent(inout) :: vdata
        logical, optional, intent(inout) :: recycle_track
        integer, optional, intent(out) :: rc
      end subroutine initialize
   end interface

end module HistoryTrajectoryMod  

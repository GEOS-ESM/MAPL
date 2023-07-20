#include "MAPL_ErrLog.h"
#include "unused_dummy.H"


submodule (HistoryTrajectoryMod)  HistoryTrajectory_implement
   contains
     module procedure HistoryTrajectory_from_config
         use pflogger, only : Logger, logging
         integer :: status
         
         character(len=ESMF_MAXSTR) :: filename
         type(NetCDF4_FileFormatter) :: formatter
         type(FileMetadataUtils) :: metadata_utils
         type(FileMetadata) :: basic_metadata
         integer(ESMF_KIND_I8) :: num_times

         integer :: ncid, grpid, ncid0
         integer :: dimid(10),  dimlen(10)
         integer :: len
         integer :: i
         character(len=ESMF_MAXSTR) :: grp_name
         character(len=ESMF_MAXSTR) :: dim_name(10)
         character(len=ESMF_MAXSTR) :: var_name_lon
         character(len=ESMF_MAXSTR) :: var_name_lat
         character(len=ESMF_MAXSTR) :: var_name_time
         integer :: time_integer, second
         type(ESMF_TimeInterval)    :: Frequency_epoch
         type(ESMF_Time)            :: currTime

         type(Logger), pointer :: lgr

         _UNUSED_DUMMY(unusable)

         ! __ parse variables, set alarm
         !
         call ESMF_ConfigGetAttribute(config, value=traj%obsFile, default="", &
              label=trim(string) // 'track_file:', _RC)
         call ESMF_ConfigGetAttribute(config, value=traj%nc_index, default="", &
              label=trim(string) // 'nc_Index:', _RC)
         call ESMF_ConfigGetAttribute(config, value=traj%nc_time, default="", &
              label=trim(string) // 'nc_Time:', _RC)
         call ESMF_ConfigGetAttribute(config, value=traj%nc_longitude, default="", &
              label=trim(string) // 'nc_Longitude:', _RC)
         call ESMF_ConfigGetAttribute(config, value=traj%nc_latitude, default="", &
              label=trim(string) // 'nc_Latitude:', _RC)

         traj%clock=clock
         call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
         call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(string)//'Epoch:', default=0, _RC)
         _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
         call hms_2_s (time_integer, second, _RC)
         call ESMF_TimeIntervalSet(frequency_epoch, s=second, _RC)
         traj%Epoch = time_integer
         traj%frequency_epoch = frequency_epoch
         traj%RingTime  = currTime
         traj%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency_epoch, &
              RingTime=traj%RingTime, sticky=.false., _RC )         

         _RETURN(_SUCCESS)

       end procedure


       
end submodule HistoryTrajectory_implement

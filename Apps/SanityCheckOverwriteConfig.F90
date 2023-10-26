program simple_overwrite_config

   use ESMF

   implicit none

   integer, parameter :: MAX_LENGTH = 1024
   character(len=*), parameter :: FILENAME = 'OverwriteConfig.rc'
   type(ESMF_Config) :: config
   character(len=MAX_LENGTH) :: val
   logical :: is_present
   integer :: stat

   write(*, *) 'SANITY CHECK'
   call ESMF_Initialize(rc=stat)
   write(*, *) 'ESMF_Initialize: ', stat
   config = ESMF_ConfigCreate(rc=stat)
   write(*, *) 'ESMF_ConfigCreate: ', stat
   call ESMF_ConfigLoadFile(config, FILENAME, rc=stat)
   write(*, *) 'ESMF_ConfigLoadFile: ', stat
   call ESMF_ConfigSetAttribute(config, value= '19991231235959', label='SwathGrid.Epoch_init:', rc=stat)
   write(*, *) 'ESMF_ConfigSetAttribute: ', stat
   call ESMF_ConfigFindLabel(config, label='SwathGrid.nc_Time:', isPresent=is_present, rc = stat)
   write(*, *) 'ESMF_ConfigFindLabel: ', is_present, stat
   call ESMF_ConfigGetAttribute(config, label='SwathGrid.nc_Time:', value=val, rc=stat) 
   write(*, *) 'ESMF_GetAttribute: ', trim(val), stat
   call ESMF_ConfigDestroy(config, rc=stat)
   write(*, *) 'ESMF_ConfigDestroy: ', stat
   call ESMF_Finalize(rc=stat)
   write(*, *) 'ESMF_Finalize: ', stat

end program simple_overwrite_config

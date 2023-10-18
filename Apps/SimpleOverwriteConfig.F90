#if defined(CHECK_STAT)
#undef CHECK_STAT
#endif
#define CHECK_STAT(C, M) if(C /= ESMF_SUCCESS) error stop M

!=========================== SIMPLE_OVERWRITE_CONFIG ===========================
! =========== Simple reproducer for ESMF_SetAttribute overwrite bug. ===========

! The config file name is hard-coded to "OverwriteConfig.rc", so
! "OverwriteConfig.rc" must exist in the directory where the application is run.
! The contents of "OverwriteConfig.rc" are found at the end of the program file.
! Attribute labels are hard-coded, so "OverwriteConfig.rc" must match the
! contents at the end of this program file.

! The test is performed in sections 4 and 5. Sections 1 to 3 are setup, and
! section 6 is teardown. So there are 6 ESMF procedure calls, but the test is 
! two ESMF procedure calls. There are return (rc) calls to verify that each
! ESMF procedure call is successful, even if the result indicates a bug.

program simple_overwrite_config

   use ESMF

   implicit none

   integer, parameter          ::   MAX_LENGTH = 64
   character(len=*), parameter ::   LABEL1 = 'SwathGrid.Epoch_init:'
   character(len=*), parameter ::   VALUE1 = '19991231T235959'
   character(len=*), parameter ::   LABEL2 = 'SwathGrid.nc_Time:'
   character(len=*), parameter ::   NEW_VALUE = '19991231235959'
   character(len=*), parameter ::   FILENAME = 'OverwriteConfig.rc'

   type(ESMF_Config) :: config
   integer :: stat
   logical :: is_present
   character(len=3) :: notmsg = 'not'


!===============================================================================
!==================================== SETUP ====================================
!===============================================================================
!  1. Initialize.
   call ESMF_Initialize(rc=stat)
   CHECK_STAT(stat, 'Initialize ESMF failed.')

!  2. Create.
   config = ESMF_ConfigCreate(rc=stat)
   CHECK_STAT(stat, 'Create config failed.')

!  3. Load.
   call ESMF_ConfigLoadFile(config, trim(FILENAME), rc=stat)
   CHECK_STAT(stat, 'Load config file failed.')


!===============================================================================
!=================================== TEST ======================================
!===============================================================================
   ! 4. Set (new) value for LABEL1.
   call ESMF_ConfigSetAttribute(config, value=VALUE1, label=LABEL1, rc=stat)
   CHECK_STAT(stat, 'Set failed for LABEL1')

   ! 5. Look for LABEL2.
   call ESMF_ConfigFindLabel(config, label=LABEL2, isPresent=is_present, rc=stat)
   CHECK_STAT(stat, 'Find failed for LABEL2')

   if(is_present) notmsg = ''
   write(*, *) 'LABEL2 is ' // trim(notmsg) // ' found.'

!===============================================================================
!================================= END of TEST =================================
!===============================================================================

!  6. Destroy.
   call ESMF_ConfigDestroy(config, rc=stat)
   CHECK_STAT(stat, 'Destroy failed for config.')

end program simple_overwrite_config

!============================= OverwriteConfig.rc ==============================
!SwathGrid.Epoch_init:	'20121113T000000'
!SwathGrid.nc_Time:	'time'
!SwathGrid.nc_Longitude:	'cell_across_swath'

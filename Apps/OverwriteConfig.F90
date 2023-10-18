program overwrite_config

   use ESMF

   implicit none

   integer, parameter ::            MAX_LENGTH = 1024
   integer, parameter ::            IOSUCCESS = 0
   character(len=*), parameter ::   LABEL1 = 'SwathGrid.Epoch_init:'
   character(len=*), parameter ::   VALUE1 = '19991231T235959'
   character(len=*), parameter ::   LABEL2 = 'SwathGrid.nc_Time:'
   character(len=*), parameter ::   FAILURE_MESSAGE = 'Test terminated prematurely: '
   logical, parameter ::            ESMF_ = .TRUE.

   type(ESMF_Config) :: config
   character(len=MAX_LENGTH) :: filename, command, new_value
   integer :: stat, length, ios
   logical :: is_present

   ! Get command line for output.
   call get_command(command, length=length, status=ios)
   if(fail(ios) .or. length < 1) command = ''

   ! Get filename.
   call get_command_argument(1, filename, length, ios)
   if(fail(ios) .or. length < 1) error stop FAILURE_MESSAGE // 'No filename supplied.'

   ! Initialize ESMF.
   call ESMF_Initialize(rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // 'ESMF_Initialize failed.'
   write(*, *) 'ESMF_Initialize completed normally.'      

   ! Create ESMF_Config.
   config = ESMF_ConfigCreate(rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // 'ESMF_ConfigCreate failed.'
   write(*, *) 'ESMF_ConfigCreate completed normally.'

   ! Load resource file.
   call ESMF_ConfigLoadFile(config, trim(filename), rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // 'ESMF_ConfigLoadFile failed.'
   write(*, *) "ESMF_ConfigLoadFile completed normally for '" // trim(filename) // "'."

!  =============================================================================
!  TEST ========================================================================
!  =============================================================================

   ! Set (new) value for LABEL1.
   call ESMF_ConfigSetAttribute(config, value=VALUE1, label=LABEL1, rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // "ESMF_ConfigSetAttribute failed for '" // LABEL1 // "'."
   write(*, *) "ESMF_ConfigSetAttribute completed normally for '" // LABEL1 // "'."

   ! Look for LABEL1.
   call ESMF_ConfigGetAttribute(config, value=new_value, label=LABEL1, rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // "ESMF_ConfigGetAttribute failed for '" // LABEL1 // "'."
   write(*, *) "ESMF_ConfigGetAttribute completed normally for '" // LABEL1 // "'."
   write(*, *) "Attribute '" // LABEL1 // "' = '" // trim(new_value) // "'."

   ! Look for LABEL2.
   call ESMF_ConfigFindLabel(config, label=LABEL2, isPresent=is_present, rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // "ESMF_ConfigFindLabel failed for '" // LABEL2 // "'."
   write(*, *) "ESMF_ConfigFindLabel completed normally for '" // LABEL2 // "'."

   if(is_present) then
      write(*, *) "'" // LABEL2 // "' found."
   else
      write(*, *) "'" // LABEL2 // "' not found."
   end if

!  =============================================================================
!  END of Test =================================================================
!  =============================================================================

   call ESMF_ConfigGetAttribute(config, value=new_value, label=LABEL2, rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // "ESMF_ConfigGetAttribute failed for '" // LABEL2 // "'."
   write(*, *) "ESMF_ConfigGetAttribute completed normally for '" // LABEL2 // "'."
   write(*, *) "Attribute '" // LABEL2 // "' = '" // trim(new_value) // "'."

   call ESMF_ConfigDestroy(config, rc=stat)
   if(fail(stat, ESMF_)) then
      write(*, *) 'ESMF_ConfigDestroy failed.'
   else 
      write(*, *) 'ESMF_ConfigDestroy complete normally'
   end if

   if(len_trim(command) > 0) then
      write(*, *) "'" // trim(command) // "'" // " completed normally."
   else
      write(*, *) 'Test completed normally.'
   end if

contains

   logical function failed(stat, successful)
      integer, intent(in) :: stat
      integer, intent(in) :: successful

      failed = .not. (stat == successful)

   end function failed

   logical function fail(stat, is_esmf)
      integer, intent(in) :: stat
      logical, optional, intent(in) :: is_esmf

      if(present(is_esmf) .and. is_esmf) then
         fail = ESMF_Fail(stat)
      else
         fail = .not. (stat == 0)
      end if

   end function fail
   
   logical function ESMF_Fail(stat)
      integer, intent(in) :: stat

      ESMF_Fail = .not. (stat == ESMF_SUCCESS)

   end function ESMF_Fail

end program overwrite_config

program overwrite_config

   use ESMF

   implicit none

   integer, parameter ::            MAX_LENGTH = 1024
   integer, parameter ::            IOSUCCESS = 0
   character(len=*), parameter ::   FIRST_LABEL = 'LABEL1:'
   character(len=*), parameter ::   SECOND_LABEL = 'LABEL2:'
   character(len=*), parameter ::   NEW_VALUE = '19991231T235959'
   character(len=*), parameter ::   FAILURE_MESSAGE = 'Test terminated prematurely: '
   logical, parameter ::            ESMF_ = .TRUE.

   type(ESMF_Config) :: config
   character(len=MAX_LENGTH) :: filename, command
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

   ! Set (new) value for FIRST_LABEL.
   call ESMF_ConfigSetAttribute(config, value=NEW_VALUE, label=FIRST_LABEL, rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // "ESMF_ConfigSetAttribute failed for '" // FIRST_LABEL // "'."
   write(*, *) "ESMF_ConfigSetAttribute completed normally for '" // FIRST_LABEL // "'."

   ! Look for SECOND_LABEL.
   call ESMF_ConfigFindLabel(config, label=SECOND_LABEL, isPresent=is_present, rc=stat)
   if(fail(stat, ESMF_)) error stop FAILURE_MESSAGE // "ESMF_ConfigFindLabel failed for '" // SECOND_LABEL // "'."
   write(*, *) "ESMF_ConfigFindLabel completed normally for '" // SECOND_LABEL // "'."

   if(is_present) then
      write(*, *) "'" // SECOND_LABEL // "' found."
   else
      write(*, *) "'" // SECOND_LABEL // "' not found."
   end if

!  =============================================================================
!  END of Test =================================================================
!  =============================================================================

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

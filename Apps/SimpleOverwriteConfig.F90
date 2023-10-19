!=========================== SIMPLE_OVERWRITE_CONFIG ===========================
! =========== Simple reproducer for ESMF_SetAttribute overwrite bug. ===========
program simple_overwrite_config

   use ESMF

   implicit none

   logical, parameter            :: KEEP_TEMPORARY_FILE = .TRUE.
   integer, parameter            :: MAX_LENGTH = 64
   integer, parameter            :: IOSUCCESS = 0
   character(len=*), parameter   :: NEW_VALUE = '19991231235959'
   character(len=*), parameter   :: DEFNAME = 'overwriteconfig.rc.tmp'
   character(len=*), parameter   :: AFMT = '(A)'
   character(len=*), parameter   :: FIRST_LABEL = 'LABEL1:'
   character(len=*), parameter   :: SECOND_LABEL = 'LABEL2:'
   character(len=*), parameter   :: THIRD_LABEL = 'LABEL3:'

   type(ESMF_Config) :: config
   integer :: stat
   character(len=MAX_LENGTH) :: filename
   logical :: label_found

!  1. Initialize.
   call ESMF_Initialize(rc=stat)
   call check(stat == ESMF_SUCCESS, 'Initialize ESMF failed.')
   call setup(config, filename)

!=================================== TEST ======================================
   ! 2. Set (new) value for first attribute.
   call ESMF_ConfigSetAttribute(config, value=NEW_VALUE, label=FIRST_LABEL, rc=stat)
   call check(stat == ESMF_SUCCESS, 'Set failed for first attribute')

   ! 3. Look for second attribute.
   call ESMF_ConfigFindLabel(config, label=SECOND_LABEL, isPresent=label_found, rc=stat)
   call check(stat == ESMF_SUCCESS, 'Find failed for second attribute')

   if(label_found) then
      write(*, fmt=AFMT) 'Second attribute is found.'
   else
      write(*, fmt=AFMT) 'Second attribute is not found.'
   end if
!================================= END TEST ====================================

   call ESMF_ConfigDestroy(config, rc=stat)
   if(stat /= ESMF_SUCCESS) write(*, fmt=AFMT) 'Destroy failed for config.'

contains

   subroutine check(condition, message)
      logical, intent(in) :: condition
      character(len=*), optional, intent(in) :: message

      if(.not. condition) then
         if(present(message)) then
            error stop message
         else
            error stop
         end if
      end if

   end subroutine check

   subroutine setup(config, filename)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(out) :: filename
      character(len=MAX_LENGTH) :: message
      logical :: successful
      integer :: stat

      ! 3. Create config.
      config = ESMF_ConfigCreate(rc=stat)
      call check(stat == ESMF_SUCCESS, 'Create config failed.')

      call get_command_argument(1, filename, status=stat)
      if(.not. (stat == IOSUCCESS)) then
         ! Generate config file.
         filename = DEFNAME
         call generate_config_file(filename, message, successful)
         call check(successful, message)
      end if

      ! 4. Load config.
      call ESMF_ConfigLoadFile(config, trim(filename), rc=stat)
      call check(stat == ESMF_SUCCESS , 'Load config file failed.')

   end subroutine setup

   subroutine generate_config_file(filename, message, successful)
      character(len=*), intent(in) :: filename
      character(len=*), intent(out) :: message
      logical, intent(out) :: successful
      integer :: iounit, i, ios
      character(len=:), allocatable :: attribute(:, :)

      write(*, fmt=AFMT) 'temporary file = ' // trim(filename)

      message = ''

      successful = (len_trim(filename) > 0)
      if(.not. successful) then
         message = 'Filename is empty.'
         return
      end if
      
      open(file=trim(filename), newunit=iounit, iostat=ios)
      successful = (ios == 0)
      if(.not. successful) then
         close(unit=iounit, iostat=ios)
         message = 'Failed to open filename ' // trim(filename)
         return
      end if

      attribute = make_attribute()
      do i = 1, size(attribute, 2)
         write(*, *) trim(attribute(1, i)) // ' ' // trim(attribute(2,i))
         write(unit=iounit, fmt='(A)', iostat=ios) trim(attribute(1, i)) // ' ' // trim(attribute(2,i))
         successful = (ios == 0)
         if(.not. successful) then
            message = 'Failed to write'
            exit
         end if
      end do

      close(unit=iounit, iostat=ios)
      successful = (ios == IOSUCCESS)

      if(successful .and. len_trim(message) == 0) then
         message = 'Config file generated successfully'
      else
         message = trim(message) // ' :: Failed to close'
      end if
      
   end subroutine generate_config_file

   function make_attribute() result(attribute)
      character(len=MAX_LENGTH) :: attribute(2,3)
      ! Each column is an attribute: [<label>, <value>]

      ! First attribute
      attribute(1, 1) = FIRST_LABEL
      attribute(2, 1) = '19991231T235959'

      ! Second attribute
      attribute(1, 2) = SECOND_LABEL     
      attribute(2, 2) = 'time'

      ! Third attribute
      attribute(1, 3) = THIRD_LABEL
      attribute(2, 3) = 'cell_across_swath'

   end function make_attribute

end program simple_overwrite_config

!============================= OverwriteConfig.rc ==============================
!LABEL1:	'20121113T000000'
!LABEL2:	'time'
!LABEL3: 	'cell_across_swath'

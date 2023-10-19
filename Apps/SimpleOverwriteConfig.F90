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
! LABEL = 1, VALUE = 2

program simple_overwrite_config

   use ESMF

   implicit none

   logical, parameter            :: KEEP_TEMPORARY_FILE = .TRUE.
   integer, parameter            :: MAX_LENGTH = 64
   integer, parameter            :: IOSUCCESS = 0
   character(len=*), parameter   :: NEW_VALUE = '19991231235959'
   character(len=*), parameter   :: DEFNAME = 'OverwriteConfig.rc.tmp'
   character(len=*), parameter   :: AFMT = '(A)'
   character(len=*), parameter   :: FIRST_LABEL = 'LABEL1:'
   character(len=*), parameter   :: SECOND_LABEL = 'LABEL2:'
   character(len=*), parameter   :: THIRD_LABEL = 'LABEL3:'

   type(ESMF_Config) :: config
   integer :: stat
   character(len=MAX_LENGTH) :: filename
   logical :: is_open, label_found, filename_found

!  1. Initialize.
   call ESMF_Initialize(rc=stat)
   call check(stat == ESMF_SUCCESS, 'Initialize ESMF failed.')
   call setup(config, filename, filename_found)

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
   call teardown(config, filename_found, KEEP_TEMPORARY_FILE, filename)

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

   subroutine setup(config, filename, filename_found)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(out) :: filename
      logical, intent(out) :: filename_found
      character(len=:), allocatable :: attribute(:,:)
      character(len=MAX_LENGTH) :: message
      logical :: successful
      integer :: stat

      ! 3. Create config.
      config = ESMF_ConfigCreate(rc=stat)
      call check(stat == ESMF_SUCCESS, 'Create config failed.')

      call get_command_argument(1, filename, status=stat)
      filename_found = (stat == IOSUCCESS)
      if(.not. filename_found) then
         ! Make attributes for ESMF_Config.
         attribute = make_attribute()

         ! Generate config file.
         filename = DEFNAME
         call generate_config_file(filename, message, successful)
         call check(successful, message)
      end if

      ! 4. Load config.
      call ESMF_ConfigLoadFile(config, trim(filename), rc=stat)
      call check(stat == ESMF_SUCCESS , 'Load config file failed.')

   end subroutine setup

   subroutine teardown(config, filename_found, keep_temp_file, filename)
      type(ESMF_Config), intent(inout) :: config
      logical, intent(in) :: filename_found, keep_temp_file
      character(len=*), intent(in) :: filename
      integer :: iounit, ios, stat

      ! 5. Destroy config.
      call ESMF_ConfigDestroy(config, rc=stat)
      if(stat /= ESMF_SUCCESS) write(*, fmt=AFMT) 'Destroy failed for config.'

      if(filename_found  .or. keep_temp_file) return

      inquire(file=trim(filename), number=iounit, iostat=ios)
      if(ios == 0) close(unit=iounit, status='DELETE', iostat=ios)
      if(ios /= 0) write(*, fmt=AFMT) 'Failed to delete temporary file ' // trim(filename)

   end subroutine teardown

   subroutine generate_config_file(filename, message, successful)
      character(len=*), intent(in) :: filename
      character(len=*), intent(out) :: message
      logical, intent(out) :: successful
      type(ESMF_Config) :: config
      character(len=MAX_LENGTH) :: label, value
      integer :: iounit, i, ios, stat = 0
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
         message = 'Failed to close filename ' // trim(filename)
         return
      end if

      ! 6. Create config (locally.)
      config = ESMF_ConfigCreate(rc=stat)
      successful = (stat == ESMF_SUCCESS)
      if(.not. successful) then
         close(unit=iounit, iostat=ios)
         message = 'Failed to create config for temporary file'
         return
      end if

      attribute = make_attribute()

      ! 7. Set attributes.
      do i = 1, size(attribute, 2)
         label = attribute(1, i)
         value = attribute(2, i)
         call ESMF_ConfigSetAttribute(config, value=trim(value), &
            label=trim(label), rc=stat)
         successful = (stat == ESMF_SUCCESS)
         if(.not. successful) exit
      end do

      if(.not. successful) then
         close(unit=iounit, iostat=ios)
         message = 'Failed to set attribute ' // trim(attribute(1, i))
         return
      end if

      ! 8. Destroy config (locally.)
      call ESMF_ConfigDestroy(config, rc=stat)
      if(stat /= ESMF_SUCCESS) message = 'Failed to destroy ESMF_Config :: '

      message = trim(message) // 'Config file generated successfully'

      close(unit=iounit, iostat=ios)
      
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

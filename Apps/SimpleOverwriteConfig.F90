program simple_overwrite_config

   use ESMF

   implicit none

   integer, parameter :: MAX_LENGTH = 1024
   integer, parameter :: SUCCESS = 0
   character(len=*), parameter :: NEW_VALUE = '19991231235959'
   character(len=*), parameter :: QUOTED_NEW_VALUE = "'" // NEW_VALUE // "'"
   character(len=*), parameter :: LABEL1 = 'SwathGrid.Epoch_init:'
   character(len=*), parameter :: VALUE1 = '20121113T000000'
   character(len=*), parameter :: LABEL2 = 'SwathGrid.nc_Time:'
   character(len=*), parameter :: VALUE2 = 'time'
   character(len=*), parameter :: LABEL3 = 'SwathGrid.nc_Longitude:'
   character(len=*), parameter :: VALUE3 = 'cell_across_swath'
   character(len=MAX_LENGTH) :: attribs(2, 3)

   type(ESMF_Config) :: config
   integer :: stat
   character(len=MAX_LENGTH) :: filename, val
   logical :: is_present

   write(*, *) 'SANITY CHECK'
   call ESMF_Initialize(rc=stat)
   write(*, *) 'ESMF_Initialize: ', stat
   config = ESMF_ConfigCreate(rc=stat)
   write(*, *) 'ESMF_ConfigCreate: ', stat
   call ESMF_ConfigLoadFile(config, 'OverwriteConfig.rc', rc=stat)
   write(*, *) 'ESMF_ConfigLoadFile: ', stat
   call ESMF_ConfigSetAttribute(config, value= '19991231235959', label='SwathGrid.Epoch_init:', rc=stat)
   write(*, *) 'ESMF_ConfigSetAttribute: ', stat
   call ESMF_ConfigFindLabel(config, label='SwathGrid.nc_Time:', isPresent=is_present, rc = stat)
   write(*, *) 'ESMF_ConfigFindLabel: ', is_present, stat
   call ESMF_ConfigDestroy(config, rc=stat)
   write(*, *) 'ESMF_ConfigDestroy: ', stat
   call ESMF_Finalize(rc=stat)
   write(*, *) 'ESMF_Finalize: ', stat

   attribs(1, 1) = LABEL1
   attribs(1, 2) = LABEL2
   attribs(1, 3) = LABEL3
   attribs(2, 1) = VALUE1
   attribs(2, 2) = VALUE2
   attribs(2, 3) = VALUE3

   ! Get valid config file name.
   call get_command_argument(1, filename, status=stat)
   write(*, *) 'filename: ' // trim(filename) // ', stat:', stat
   if(stat /= SUCCESS) error stop 'Failed to get filename'
   if(len_trim(filename) == 0) error stop 'Filename empty'

   ! 1. Initialize ESMF.
   call ESMF_Initialize(rc=stat)
   write(*, *) 'ESMF_Initialize stat:', stat
   if(stat /= ESMF_SUCCESS) error stop 'Failed to initialize ESMF'

   ! 2. Create config.
   config = ESMF_ConfigCreate(rc=stat)
   write(*, *) 'ESMF_ConfigCreate stat:', stat
   if(stat /= ESMF_SUCCESS) error stop 'Failed to create config'

   ! 3. Load config from file.
   call ESMF_ConfigLoadFile(config, trim(filename), rc=stat)
   write(*, *) 'ESMF_ConfigLoadFile stat:', stat
   if(stat /= ESMF_SUCCESS) error stop 'Failed to load config'

   ! Show attributes before.
   write(*, *)
   write(*, *) 'Print attributes (before):'
   call print_attributes(attribs)
   write(*, *)


   ! 4. SET FIRST ATTRIBUTE.
   write(*, *) 'BEGIN TEST'
   call ESMF_ConfigSetAttribute(config, value=quote(NEW_VALUE), label=LABEL1, rc=stat)
   write(*, *) 'ESMF_ConfigSetAttribute attribute 1 stat:', stat
   if(stat /= ESMF_SUCCESS) error stop 'Failed to change value of attribute 1'

   ! Check first attribute was set correctly.
   call ESMF_ConfigGetAttribute(config, value=val, label=LABEL1, rc=stat)
   write(*, *) 'ESMF_ConfigGetAttribute attribute 1: ' // trim(val) // ', stat:'  , stat
   write(*, *) 'actual 1: ' // trim(val) // ', expected 1: ' // NEW_VALUE
   if(stat /= ESMF_SUCCESS) error stop 'Failed to get new value of attribute 1 from config'
   if(trim(val) /= trim(quote(NEW_VALUE))) write(*, *) 'First attribute value does not match value set.'
   attribs(2, 1) = trim(val)

   ! 5. FIND SECOND LABEL (ATTRIBUTE).
   call ESMF_ConfigFindLabel(config, label=LABEL2, isPresent=is_present, rc = stat)
   write(*, *) 'ESMF_ConfigFindLabel: ', is_present, ', stat:', stat
   if(stat /= ESMF_SUCCESS) error stop 'Failed find of attribute 2'

   ! Test Result
   if(is_present) then
      write(*, *) 'TEST: Label 2 found.'
   else
      write(*, *) 'TEST: Label 2 not found.'
   end if
   write(*, *) 'END TEST'

   ! Show attributes after.
   write(*, *) 
   write(*, *) 'Print attributes (after):'
   call print_attributes(attribs)
   write(*, *) 

   ! 6. Destroy config.
   call ESMF_ConfigDestroy(config, rc=stat)
   write(*, *) 'ESMF_ConfigDestroy stat:', stat
   if(stat /= ESMF_SUCCESS) error stop 'Failed to destroy config'

   ! 7. Finalize ESMF.
   call ESMF_Finalize(rc=stat)
   write(*, *) 'ESMF_Finalize stat:', stat
   if(stat /= ESMF_SUCCESS) write(*, *) 'Failed to finalize ESMF'

contains

   ! Put quotes around string s.
   function quote(s, use_double) result(quoted)
      character(len=*), intent(in) :: s
      logical, optional, intent(in) :: use_double
      character(len=:), allocatable :: quoted

      if(present(use_double) .and. use_double) then
         quoted = '"' // s // '"'
      else
         quoted = "'" // s // "'"
      end if

   end function quote

   ! Print attributes in array. Label is first row; value is second row.
   subroutine print_attributes(attribs)
      character(len=*), intent(in) :: attribs(:, :)
      integer :: i
      character(len=*), parameter :: FMT2A = '(A, " ", A)'

      do i = 1, size(attribs, 2)
         write(*, fmt=FMT2A) trim(attribs(1, i)), trim(attribs(2, i))
      end do

   end subroutine print_attributes

end program simple_overwrite_config

!============================= OverwriteConfig.rc ==============================
! The config file must have attrbute labels identical and in the same order as
! the following lines (without '!'):
! SwathGrid.Epoch_init: '20121113T000000'
! SwathGrid.nc_Time: 'time'
! SwathGrid.nc_Longitude: 'cell_across_swath'

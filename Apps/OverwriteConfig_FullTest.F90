program overwrite_config_fulltest

   use ESMF

   implicit none

   integer, parameter :: SUCCESS = ESMF_SUCCESS
   integer, parameter :: FAILURE = SUCCESS - 1
   integer, parameter :: IOSUCCESS = 0
   character(len=*), parameter :: FMTA = '(A)'
   character(len=*), parameter :: FMTAA = '(A, " = ", A)'
   character(len=*), parameter :: FMTAI = '(A,I6)'
   character(len=*), parameter :: FMTALI = '(A, L1, I6)'
   character(len=80)  :: filename
   integer :: stat, length
   logical :: create = .FALSE.

   call get_command_argument(1, filename, length=length, status=stat)
   create = (length < 1 .or. stat /= SUCCESS)
   if(create) write(*, *) 'CREATE: ', create
   if(create) filename = 'overwrite_config.rc.tmp'
   write(*, fmt=FMTA) 'filename ' // trim(filename)
   call main(trim(filename), create)

contains

   subroutine main(filename, create)
      character(len=*), intent(in) :: filename
      logical, optional, intent(in) :: create
      character(len=*), parameter :: LABEL1 = 'SwathGrid.Epoch_init:'
      character(len=*), parameter :: VALUE1 = '20121113T000000'
      character(len=*), parameter :: LABEL2 = 'SwathGrid.nc_Time:'
      character(len=*), parameter :: VALUE2 = 'time'
      character(len=*), parameter :: LABEL3 = 'SwathGrid.nc_Longitude:'
      character(len=*), parameter :: VALUE3 = 'cell_across_swath'
      character(len=*), parameter :: NEW_VALUE = '19991231235959'
      type(ESMF_Config) :: config
      character(len=80) :: attribute(2,3)
      integer :: stat, i

      write(*, fmt=FMTA) 'MAIN ==========================================================================='

      if(present(create) .and. create) then
         write(*, FMTA) 'Creating ' // trim(filename)
         attribute(1, 1) = LABEL1
         attribute(1, 2) = LABEL2
         attribute(1, 3) = LABEL3
         attribute(2, 1) = VALUE1
         attribute(2, 2) = VALUE2
         attribute(2, 3) = VALUE3

         write(*, FMTA) 'attribute:'
         do i = 1, size(attribute, 2)
            write(*, fmt=FMTA) trim(attribute(1, i)) // " " // trim(attribute(2, i))
         end do 

         call write_config_file(filename, attribute, rc=stat)
         if(stat /= IOSUCCESS) write(*, fmt=FMTAI) 'Failed to write config file', stat
      end if

      call ESMF_Initialize(rc=stat)
      write(*, fmt=FMTAI) 'initialize', stat

      call setup(config, filename, LABEL1, LABEL2, LABEL3, rc=stat)
      write(*, fmt=FMTAI) 'SETUP', stat
      
      call test_overwrite(config, LABEL1, NEW_VALUE, LABEL2, rc=stat)
      write(*, fmt=FMTAI) 'TEST', stat

      call ESMF_Finalize(rc=stat)
      write(*, fmt=FMTAI) 'finalize', stat

      write(*, fmt=FMTA) 'END MAIN ======================================================================='
   end subroutine main

   subroutine setup(config, filename, label_1, label_2, label_3, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: filename, label_1, label_2, label_3
      integer, intent(out) :: rc
      integer :: stat
      logical :: config_created, is_present!, make_message, 
      logical :: setup_succeeded

      write(*, fmt=FMTA) 'SETUP ========================================================================='

      config = ESMF_ConfigCreate(rc=stat)
      write(*, fmt=FMTAI) 'create', stat
      setup_succeeded = (stat == SUCCESS)

      config_created = ESMF_ConfigIsCreated(config, rc=stat)
      write(*, fmt=FMTALI) 'is created ', config_created, stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)

      call ESMF_ConfigLoadFile(config, filename, rc=stat)
      write(*, fmt=FMTAI) 'load ' // filename, stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)
      
      call ESMF_ConfigFindLabel(config, label=label_1, isPresent=is_present, rc=stat)
      write(*, fmt=FMTALI) 'Label 1 ' // label_1 // ' is present ', is_present, stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)

      call ESMF_ConfigFindLabel(config, label=label_2, isPresent=is_present, rc=stat)
      write(*, fmt=FMTALI) 'Label 2 ' // label_2 // ' is present ', is_present, stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)

      call ESMF_ConfigFindLabel(config, label=label_3, isPresent=is_present, rc=stat)
      write(*, fmt=FMTALI) 'Label 3 ' // label_3 // ' is present ', is_present, stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)

      if(setup_succeeded) then
         rc = SUCCESS
      else
         rc = FAILURE
      end if

      write(*, fmt=FMTA) 'END SETUP ====================================================================='

   end subroutine setup

   subroutine test_overwrite(config, label1, newval, label2, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label1, newval, label2
      integer, intent(out) :: rc
      logical :: is_present, test_succeeded
      integer :: stat

      write(*, fmt=FMTA) 'TEST =========================================================================='

      call ESMF_ConfigSetAttribute(config, value=newval, label=label1, rc=stat)
      write(*, fmt=FMTAI) 'Set label1 ' // label1 // ' = ' // newval, stat
      test_succeeded = (stat == SUCCESS)

      call ESMF_ConfigFindLabel(config, label=label2, isPresent=is_present, rc = stat)
      write(*, fmt=FMTALI) 'Label2 ' // label2 // ' is present ', is_present, stat
      test_succeeded = test_succeeded .and. (stat == SUCCESS)

      write(*, fmt=FMTA) 'END TEST ======================================================================'

      if(test_succeeded) then
         rc = SUCCESS
      else
         rc = FAILURE
      end if

   end subroutine test_overwrite

   function quote(s, use_double) result(quoted)
      character(len=*), intent(in) :: s
      logical, optional, intent(in) :: use_double
      character(len=:), allocatable :: quoted

      if(present(use_double) .and. use_double) then
         quoted = '"' // trim(adjustl(s)) // '"'
      else
         quoted = "'" // trim(adjustl(s)) // "'"
      end if

   end function quote

   function make_filename() result(fname)
      character(len=:), allocatable :: fname
      character(len=32) :: date, time
      call date_and_time(date=date, time=time)
      fname = 'overwrite_config_' // trim(date) // trim(clean(time)) // '_rc.tmp'
   end function make_filename

   function clean(s) result(cl)
      character(len=*), intent(in) :: s
      character(len=len(s)) :: cl
      character(len=*), parameter :: FMTAI = '(A, I2)'
      character(len=*), parameter :: SRCH = '.:/,+-Z '
      character, parameter :: REPL = '_'
      integer :: i, k

      cl = adjustl(s)
      k = len_trim(cl)
      i = scan(cl(1:k), SRCH, back=.TRUE.)
      do while(i > 0)
         cl(i:i) = '_'
         k = i-1
         i = scan(cl(1:k), SRCH, back=.TRUE.)
      end do

   end function clean

   subroutine write_config_file(filename, attr, rc)
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: attr(:,:)
      integer, intent(out) :: rc
      integer :: i, iounit, ios

      write(*,  FMTA) 'WRITE CONFIG FILE =============================================================='
      open(file=trim(filename), newunit=iounit, iostat=ios)
      if(ios /= IOSUCCESS) then
         write(*, fmt=FMTA) 'Failed to open ' // trim(filename)
         rc = ios
         return
      end if
      write(*, FMTA) 'Open successful'

      rc = IOSUCCESS
      do i = 1, size(attr, 2)
         write(unit=iounit, fmt='(A," ",A)', iostat=ios) trim(attr(1, i)), quote(attr(2, i),  use_double=.TRUE.) 
         if(ios /= IOSUCCESS) then
            write(*, fmt=FMTAI) 'Failed to write attribute ', i
            rc = rc - 1
            exit
         end if
         write(*, fmt='(A," ",A)', iostat=ios) 'Wrote ' // trim(attr(1, i)), quote(attr(2, i),  use_double=.TRUE.) // ' to file ' // trim(filename)
      end do

      close(unit=iounit, status='KEEP', iostat=ios)
      if(ios /= IOSUCCESS) write(*, fmt=FMTA) 'Failed to close ' // trim(filename)

      write(*,  FMTA) 'END WRITE CONFIG FILE =========================================================='

   end subroutine write_config_file

end program overwrite_config_fulltest

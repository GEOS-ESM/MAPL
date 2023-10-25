#define I_AM_MAIN
#include "MAPL_Generic.h"

program  test_platform
  use ESMF
  use MAPL
  use Fortran_read_file
  use obs_platform

  implicit none
  type(ESMF_VM) :: vm
  integer  unitr
  integer  status, rc, count
  type(ESMF_Config)           :: cf
  character(len=ESMF_MAXSTR)  :: HIST_CF
  character (len=ESMF_MAXSTR) :: fname
  character (len=ESMF_MAXSTR) :: marker
  character (len=ESMF_MAXSTR) :: line  
  type(platform), allocatable :: PLFS(:)
  integer :: k, i, j


  namelist /input/   fname
  ! -- note: work on HEAD node
  !

  read (5, nml=input)
  write(6,*) 'input fname = ', trim(fname)

  call ESMF_Initialize(vm=vm, rc=rc)
  rc=0   
  write(6,121) 'pt1'
  cf = ESMF_ConfigCreate(rc=rc) 
  write(6,121) 'pt2'
  call ESMF_ConfigLoadFile( cf, fname, unique = .true., rc = rc)   

  call ESMF_ConfigGetAttribute(cf, value=HIST_CF, &
       label="HIST_CF:", default="HIST.rc", _RC )
  unitr = GETFILE(HIST_CF, FORM='formatted', _RC)
  !!unitr = GETFILE(fname, FORM='formatted', _RC)

  
  call scan_count_match_bgn (unitr, 'PLATFORM.', count, .false.)
  rewind(unitr)
  write(6,*) 'count PLATFORM.', count
  if (count==0) then
     rc = 0
     !!return
  endif
  allocate (PLFS(count))


  do k=1, count
     call scan_begin(unitr, 'PLATFORM.', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, 'PLATFORM.')
     j=index(line, ':')
     PLFS(k)%name = line(i:j-1)
     marker=line(1:j)

     write(6,102)  'marker=', trim(marker)
     call scan_contain(unitr, marker, .true.)
     call scan_contain(unitr, 'longitude:', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, 'longitude:')
     PLFS(k)%nc_lon = trim(line(i:))
     write(6,*) 'line1 = ', trim(line)


     call scan_contain(unitr, marker, .true.)     
     call scan_contain(unitr, 'latitude:', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, 'latitude:')
     PLFS(k)%nc_lat = trim(line(i:))
     write(6,*) 'line2 = ', trim(line)

     
     call scan_contain(unitr, marker, .true.)     
     call scan_contain(unitr, 'time:', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, 'time:')
     PLFS(k)%nc_time = trim(line(i:))

     call scan_contain(unitr, marker, .true.)     
     call scan_contain(unitr, 'file_name_template:', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, 'file_name_template:')
     PLFS(k)%file_name_template = trim(line(i:))     

     write(6,102) 'ck  PLFS(k) ', &
          trim( PLFS(k)%name ), &
          trim( PLFS(k)%nc_lon ), &
          trim( PLFS(k)%nc_lat ), &
          trim( PLFS(k)%nc_time ), &
          trim( PLFS(k)%file_name_template )
  end do
  
  
  include '/Users/yyu11/sftp/myformat.inc'      

end program test_platform

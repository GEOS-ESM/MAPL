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
  character (len=ESMF_MAXSTR) :: HIST_CF
  character (len=ESMF_MAXSTR) :: fname
  character (len=ESMF_MAXSTR) :: marker
  character (len=ESMF_MAXSTR) :: line
  character (len=ESMF_MAXSTR), allocatable :: str_piece(:)  
  type(platform), allocatable :: PLFS(:)
  integer :: k, i, j
  integer :: ios, ngeoval, nplf
  integer :: length_mx
  integer :: mxseg
  integer :: nseg
    
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
  nplf = count
  allocate (PLFS(count))

  ! __ s1. scan platform name + nc_lat ...
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
     i=index(line, ':')
     PLFS(k)%nc_lon = trim(line(i+1:))

     call scan_contain(unitr, marker, .true.)     
     call scan_contain(unitr, 'latitude:', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, ':')
     PLFS(k)%nc_lat = trim(line(i+1:))
     
     call scan_contain(unitr, marker, .true.)     
     call scan_contain(unitr, 'time:', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, ':')
     PLFS(k)%nc_time = trim(line(i+1:))

     call scan_contain(unitr, marker, .true.)     
     call scan_contain(unitr, 'file_name_template:', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, ':')
     PLFS(k)%file_name_template = trim(line(i+1:))     

     write(6,102) 'ck  PLFS(k) ', &
          trim( PLFS(k)%name ), &
          trim( PLFS(k)%nc_lon ), &
          trim( PLFS(k)%nc_lat ), &
          trim( PLFS(k)%nc_time ), &
          trim( PLFS(k)%file_name_template )
  end do

  
  ! __ s2.1 scan fields: get ngeoval / nseg = nword
  length_mx = ESMF_MAXSTR
  mxseg = 10 
  allocate (str_piece(mxseg))
  rewind(unitr)
  do k=1, count
     call scan_begin(unitr, 'PLATFORM.', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, 'PLATFORM.')
     j=index(line, ':')
     PLFS(k)%name = line(i:j-1)
     marker=line(1:j)
     write(6,102)  'marker=', trim(marker)

     call scan_begin(unitr, marker, .true.)     
     call scan_contain(unitr, 'geovals_fields:', .false.)
     ios=0
     ngeoval=0
     do while (ios == 0)
        read (unitr, '(A)' ) line
        write(6,*) 'field line:', trim(line)
        i=index(line, '::')
        if (i==0) then
           ngeoval = ngeoval + 1
           call  split_string_by_space (line, length_mx, mxseg, &
                nseg, str_piece, status)
           write(6,*) 'nseg', nseg
           write(6,*) 'str_piece(1:nseg)', str_piece(1:nseg)
        else
           exit
        endif
     enddo
     PLFS(k)%ngeoval = ngeoval
     write(6,*)  'ngeoval=', ngeoval     
     allocate ( PLFS(k)%field_name (nseg, ngeoval) )
  end do

  
  ! __ s2.2 scan fields: get splitted  PLFS(k)%field_name
  rewind(unitr)
  do k=1, count
     call scan_begin(unitr, 'PLATFORM.', .false.)
     backspace(unitr)
     read(unitr, '(a)') line
     i=index(line, 'PLATFORM.')
     j=index(line, ':')
     PLFS(k)%name = line(i:j-1)
     marker=line(1:j)
     write(6,102)  'marker=', trim(marker)
     !
     call scan_begin(unitr, marker, .true.)     
     call scan_contain(unitr, 'geovals_fields:', .false.)
     ios=0
     ngeoval=0
     do while (ios == 0)
        read (unitr, '(A)' ) line
        write(6,*) 'field line:', trim(line)
        i=index(line, '::')
        if (i==0) then
           ngeoval = ngeoval + 1
           call  split_string_by_space (line, length_mx, mxseg, &
                nseg, str_piece, status)
           PLFS(k)%field_name (1:nseg, ngeoval) = str_piece(1:nseg)
        else
           exit
        endif
     enddo
  end do

  do k=1, nplf
     do i=1, ngeoval
        do j=1, nseg
           write(6,*) 'PLFS(k)%field_name (1:nseg, ngeoval)=', trim(PLFS(k)%field_name (j,i))
        enddo
     enddo
  enddo
  
  include '/Users/yyu11/sftp/myformat.inc'      

end program test_platform

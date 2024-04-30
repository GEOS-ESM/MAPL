!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_HistoryGridCompMod`
!
! Author: GMAO SI-Team
!
! `MAPL_HistoryGridCompMod` contains the `Initialize`, `Run` and `Finalize` methods for `History`.
! The three methods are called at the level of CAP.
!
  SUBMODULE (MAPL_HistoryGridCompMod) regen_rcx_for_obs_platform
!
! !USES:
!
  implicit none

contains

  ! __ read data to object: obs_platform
  ! __ for each collection: find union fields, write to collection.rcx
  ! __ note: this subroutine is called by MPI root only
  !
  ! __ note: this subroutine is called by MPI root only
  !
  MODULE subroutine regen_rcx_for_obs_platform (config, nlist, list, rc)

    use  MAPL_scan_pattern_in_file
    use MAPL_ObsUtilMod, only : obs_platform, union_platform

   intrinsic :: size
   intrinsic :: TRIM


    !
    !  Plan:
    !- read and write  schema
    !- extract union of field lines, print out to rc
    integer, parameter :: ESMF_MAXSTR2 = 2*ESMF_MAXSTR
    type(ESMF_Config), intent(inout)       :: config
    integer, intent(in)                    :: nlist
    type(HistoryCollection), pointer       :: list(:)
    integer, intent(inout), optional :: rc

    character(len=ESMF_MAXSTR) :: HIST_CF
    integer :: n, unitr, unitw
    logical :: match, contLine, con, con2
    integer :: status

    character (len=ESMF_MAXSTR) :: marker
    character (len=ESMF_MAXSTR) :: string
    character (len=ESMF_MAXSTR2) :: line, line2
    character (len=ESMF_MAXSTR2), allocatable :: str_piece(:)
    type(obs_platform), allocatable :: PLFS(:)
    type(obs_platform) :: p1
    integer :: k, i, j, m, i2
    integer :: ios, ngeoval, count, nplf
    integer :: length_mx
    integer :: mxseg
    integer :: nseg
    integer :: nseg_ub
    integer :: nfield, nplatform
    integer :: nentry_name
    logical :: obs_flag
    integer, allocatable :: map(:)
    type(Logger), pointer          :: lgr

    lgr => logging%get_logger('HISTORY.sampler')

    !
    !
    call ESMF_ConfigGetAttribute(config, value=HIST_CF, &
         label="HIST_CF:", default="HIST.rc", _RC )
    unitr = GETFILE(HIST_CF, FORM='formatted', _RC)

    call scan_count_match_bgn (unitr, 'PLATFORM.', nplf, .false.)
    rewind(unitr)

    if (nplf==0) then
       rc = 0
       return
    endif
    allocate (PLFS(nplf))
    allocate (map(nplf))

    ! __ global set for call split_string by space
    length_mx = ESMF_MAXSTR2
    mxseg = 100


    ! __ s1. scan get  platform name + index_name_x  var_name_lat/lon/time
    do k=1, nplf
       call scan_begin(unitr, 'PLATFORM.', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, '.')
       j=index(line, ':')
       _ASSERT(i>1 .AND. j>1, 'keyword PLATFORM.X is not found')
       PLFS(k)%name = line(i+1:j-1)
       marker=line(1:j)

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'index_name_x:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%index_name_x = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'var_name_lon:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%var_name_lon = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'var_name_lat:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%var_name_lat = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'var_name_time:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%var_name_time = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'file_name_template:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%file_name_template = trim(line(i+1:))

       call lgr%debug('%a %a %a %a %a', &
            trim( PLFS(k)%name ), &
            trim( PLFS(k)%var_name_lon ), &
            trim( PLFS(k)%var_name_lat ), &
            trim( PLFS(k)%var_name_time ), &
            trim( PLFS(k)%file_name_template ) )

    end do



    ! __ s2.1 scan fields: only determine ngeoval / nentry_name = nword
    allocate (str_piece(mxseg))
    rewind(unitr)
    do k=1, nplf
       call scan_begin(unitr, 'PLATFORM.', .false.)
       call scan_contain(unitr, 'geovals_fields:', .false.)
       ios=0
       ngeoval=0
       nseg_ub=0
       do while (ios == 0)
          read (unitr, '(A)', iostat=ios) line
          _ASSERT (ios==0, 'read line failed')
          con = (adjustl(trim(line))=='::')
          if (con) exit
          !! print *, 'line, con', trim(line), con
          con2= (index ( adjustl(line), '#' ) == 1)    ! skip comment line
          if ( .not. con2 ) then
             ngeoval = ngeoval + 1
             call  split_string_by_space (line, length_mx, mxseg, &
                  nseg, str_piece, status)
             nseg_ub = max(nseg_ub, nseg)
          end if
       enddo
       PLFS(k)%ngeoval = ngeoval
       PLFS(k)%nentry_name = nseg_ub
       allocate ( PLFS(k)%field_name (nseg_ub, ngeoval) )
       PLFS(k)%field_name = ''
       !! print*, 'k, ngeoval, nentry_name', k, ngeoval, nseg_ub
    end do


    ! __ s2.2 scan fields: get splitted PLFS(k)%field_name
    rewind(unitr)
    do k=1, nplf
       call scan_begin(unitr, 'PLATFORM.', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, 'PLATFORM.')
       j=index(line, ':')
       marker=line(1:j)
       !
       call scan_begin(unitr, marker, .true.)
       call scan_contain(unitr, 'geovals_fields:', .false.)
       ios=0
       ngeoval=0
       do while (ios == 0)
          read (unitr, '(A)', iostat=ios) line
          _ASSERT (ios==0, 'read line failed')
          !! write(6,*) 'k in nplf, line', k, trim(line)
          con = (adjustl(trim(line))=='::')
          if (con) exit
          con2= (index ( adjustl(line), '#' ) == 1)    ! skip comment line
          if (.NOT.con2) then
             ngeoval = ngeoval + 1
             call  split_string_by_space (line, length_mx, mxseg, &
                  nseg, str_piece, status)
             do m=1, nseg
                PLFS(k)%field_name (m, ngeoval) = trim(str_piece(m))
             end do
          endif
       enddo
    end do
    deallocate(str_piece)
    rewind(unitr)

    call lgr%debug('%a %i8','count PLATFORM.', nplf)
    if (mapl_am_i_root()) then
       do k=1, nplf
          write(6, '(10x,a,i3,a,2x,a)') 'PLFS(', k, ') =',  trim(PLFS(k)%name)
          do i=1, size(PLFS(k)%field_name, 2)
             line=''
             do j=1, size(PLFS(k)%field_name, 1)
                write(line2, '(a)')  trim(PLFS(k)%field_name(j,i))
                line=trim(line)//trim(line2)
             end do
             write(6, '(24x,a)') trim(line)
          enddo
       enddo
    end if
!!    write(6,*) 'nlist=', nlist


    ! __ s3: Add more entry:  'obs_files:' and 'fields:' to rcx
    !  for each collection
    obs_flag=.false.
    do n = 1, nlist
       rewind(unitr)
       string = trim( list(n)%collection ) // '.'
       unitw = GETFILE(trim(string)//'rcx', FORM='formatted', _RC)
       match = .false.
       contLine = .false.
       obs_flag = .false.
       do while (.true.)
          read(unitr, '(A)', iostat=ios, end=1236) line
          _ASSERT (ios==0, 'read line failed')
          j = index( adjustl(line), trim(adjustl(string)) )
          match = (j == 1)
          if (match) then
             j = index(line, trim(string)//'fields:')
             contLine = (j > 0)
          end if
          if (match .or. contLine) then
             write(unitw,'(A)') trim(line)
          end if
          if (contLine) then
             if (adjustl(line) == '::') contLine = .false.
          end if
          if ( index(adjustl(line), trim(string)//'ObsPlatforms:') == 1 ) then
             obs_flag =.true.
             line2 = line
             !! write(6,*) 'first line for ObsPlatforms:=', trim(line)
          endif
       end do
1236   continue


       if (obs_flag) then
          allocate (str_piece(mxseg))
          i = index(line2, ':')
          line = adjustl ( line2(i+1:) )
          call split_string_by_space (line, length_mx, mxseg, &
               nplatform, str_piece, status)

          !! to do: add debug
          !write(6,*) 'line for obsplatforms=', trim(line)
          !write(6,*) 'split string,  nplatform=', nplatform
          !write(6,*) 'nplf=', nplf
          !write(6,*) 'str_piece=', str_piece(1:nplatform)


          !
          !   a) union the platform
          !
          ! find the index for each str_piece
          map(:) = -1
          do i=1, nplatform  ! for loc collection
             do j=1, nplf    ! tot
                if ( trim(str_piece(i)) == trim( PLFS(j)%name ) ) then
                   map(i)=j
                   exit
                end if
             end do
          end do
          deallocate(str_piece)
          !! write(6,*) 'collection n=',n, 'map(:)=', map(:)

          ! __ write common nc_index,time,lon,lat
          k=map(1)   ! plat form # 1
          write(unitw, '(2(2x,a))') trim(string)//'index_name_x:    ', trim(adjustl(PLFS(k)%index_name_x))
          write(unitw, '(2(2x,a))') trim(string)//'var_name_time:   ', trim(adjustl(PLFS(k)%var_name_time))
          write(unitw, '(2(2x,a))') trim(string)//'var_name_lon:    ', trim(adjustl(PLFS(k)%var_name_lon))
          write(unitw, '(2(2x,a))') trim(string)//'var_name_lat:    ', trim(adjustl(PLFS(k)%var_name_lat))

          do i=1, nplatform
             k=map(i)
             if (i==1) then
                p1 = PLFS(k)
             else
                p1 = union_platform(p1, PLFS(k), _RC)
             end if
          end do

          nfield = p1%ngeoval
          nentry_name = p1%nentry_name
          do j=1, nfield
             line=''
             do i=1, nentry_name
                line = trim(line)//' '//trim(p1%field_name(i,j))
             enddo
              if (j==1) then
                write(unitw, '(10(2x,a))') trim(string)//'fields:', trim(line)
             else
                write(unitw, '(12x,a)') trim(line)
             end if
          end do
          write(unitw,'(a,/)') '::'
          write(unitw,'(a)') trim(string)//'obs_files:     # table start from next line'

          !! TODO: add debug
          !! write(6,*) 'nplatform', nplatform
          do i2=1, nplatform
             k=map(i2)
             write(unitw, '(a)') trim(adjustl(PLFS(k)%file_name_template))
             do j=1, PLFS(k)%ngeoval
                line=''
                do i=1, nentry_name
                   line = trim(line)//' '//trim(adjustl(PLFS(k)%field_name(i,j)))
                enddo
                write(unitw, '(a)') trim(adjustl(line))
             enddo
             write(unitw, '(20a)') (('-'), j=1,20)
          enddo
          write(unitw,'(a)') '::'
       end if
       call free_file(unitw, _RC)
    end do
    call free_file(unitr, _RC)

    _RETURN(ESMF_SUCCESS)
  end subroutine regen_rcx_for_obs_platform

END SUBMODULE

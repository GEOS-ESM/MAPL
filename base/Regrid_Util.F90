#include "MAPL_Generic.h"

   Program ut_ReGridding

   use ESMF
   use ESMFL_Mod
   use MAPL_Profiler
   use MAPL_BaseMod
   use MAPL_MemUtilsMod
   use MAPL_CFIOMod
   use MAPL_CommsMod
   use MAPL_ShmemMod
   use ESMF_CFIOMod
   use ESMF_CFIOUtilMod
   use ESMF_CFIOFileMod
   use MAPL_NewRegridderManager
   use MAPL_AbstractRegridderMod
   use mapl_RegridMethods
   use MAPL_GridManagerMod
   use MAPL_LatLonGridFactoryMod, only: LatLonGridFactory
   use MAPL_CubedSphereGridFactoryMod, only: CubedSphereGridFactory
   use MAPL_TripolarGridFactoryMod, only: TripolarGridFactory
   use MAPL_ConstantsMod, only: MAPL_PI_R8
   use MAPL_ExceptionHandling
   use MAPL_ApplicationSupport
   use pFIO
   use MAPL_ESMFFieldBundleWrite
   use MAPL_ESMFFieldBundleRead
   use MAPL_ServerManager
   use MAPL_FileMetadataUtilsMod
 
   implicit NONE

   real, parameter :: cs_stretch_uninit = -1.0
   type(DistributedProfiler), target :: t_prof
   type (ProfileReporter) :: reporter

   include "mpif.h"

   call main()
    
CONTAINS
 
    subroutine main()

!CONTAINS

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)     :: grid_new
   type(ESMF_VM)       :: vm             ! ESMF Virtual Machine

   character(len=ESMF_MAXPATHLEN) ::  Filename,OutputFile,tp_filein,tp_fileout

!  Basic information about the parallel environment
!         PET = Persistent Execution Threads
!  In the current implementation, a PET is equivalent 
!  to an MPI process
!  ------------------------------------------------
   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc
   integer :: Nx,Ny,nargs
   integer :: IM_World_new, JM_World_new, lm_world
   character(len=:), allocatable :: lev_name

   type(ESMF_FieldBundle) :: bundle
   type(ESMF_Time) :: time
   type(ESMF_Time), allocatable :: tSeries(:)
   type(ESMF_TimeInterval) :: timeInterval
   type(ESMF_Clock) :: clock

   logical :: fileCreated,file_exists

   character(len=ESMF_MAXSTR) :: RegridMth

   character(len=ESMF_MAXSTR) :: Iam

   integer :: second,minute,hour,day,month,year,itime(2),tsteps,i

   character(len=2) :: pole_new,dateline_new
   logical :: onlyVars, allTimes, newCube
   character(len=512) :: vars
   character(len=ESMF_MAXSTR) :: gridname
   character(len=ESMF_MAXPATHLEN) :: str,astr
   character(len=:), allocatable :: tripolar_file_in,tripolar_file_out
   type(ESMF_CONFIG) :: cfoutput
   integer :: regridMethod,tint
   real :: cs_stretch_param(3)
   integer :: deflate, shave
   type (FileMetaDataUtils) :: metadata
   type (FileMetaData) :: basic_metadata

   type(FieldBundleWriter) :: newWriter
   ! W.J note: I cannot see how o_server and i_server are used here
   ! but one manager is enough. It has i_server and o_server components
   type(ServerManager) :: io_server
   !type(ServerManager) :: o_server
   !type(ServerManager) :: i_server
   type(NetCDF4_FileFormatter) :: formatter
 
    Iam = "ut_ReGridding"

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF's automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_NONE, vm=vm, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet(vm, localPET=myPET, petCount=nPet)
    call MAPL_Initialize(__RC__)
    t_prof=DistributedProfiler('Regrid_Util',MpiTimerGauge(),MPI_COMM_WORLD)
    call t_prof%start(__RC__) 

    call io_server%initialize(mpi_comm_world)
    !call o_server%initialize(mpi_comm_world)
    !call i_server%initialize(mpi_comm_world)

    nx=1
    ny=6
    onlyvars=.false.
    alltimes=.true.
    regridMth='bilinear'
    newCube=.true.
    cs_stretch_param=cs_stretch_uninit
    shave=64
    deflate=0
    nargs = command_argument_count()
    do i=1,nargs
      call get_command_argument(i,str)
      select case(trim(str))
      case ('-o')
         call get_command_argument(i+1,outputfile)
      case('-i')
         call get_command_argument(i+1,filename)
      case('-ogrid')
         call get_command_argument(i+1,Gridname)
      case('-nx')
         call get_command_argument(i+1,astr)
         read(astr,*)nx
      case('-ny')
         call get_command_argument(i+1,astr)
         read(astr,*)ny
      case('-vars')
         call get_command_argument(i+1,vars)
         onlyVars = .true.
      case('-t')
         call get_command_argument(i+1,astr)
         read(astr,*)itime(1)
         call get_command_argument(i+2,astr)
         read(astr,*)itime(2)
         alltimes=.false.
      case('-stretch_factor')
         call get_command_argument(i+1,astr)
         read(astr,*)cs_stretch_param(1)
         call get_command_argument(i+2,astr)
         read(astr,*)cs_stretch_param(2)
         call get_command_argument(i+3,astr)
         read(astr,*)cs_stretch_param(3)
      case('-method')
         call get_command_argument(i+1,RegridMth)
      case('-cubeFormat')
         call get_command_argument(i+1,astr)
         if (trim(astr) == 'new') newCube=.true.
         if (trim(astr) == 'old') newCube=.false.
      case('-tp_in')
         call get_command_argument(i+1,tp_filein)
         tripolar_file_in = tp_filein
      case('-tp_out')
         call get_command_argument(i+1,tp_fileout)
         tripolar_file_out = tp_fileout
      case('-shave')
         call get_command_argument(i+1,astr)
         read(astr,*)shave
      case('-deflate')
         call get_command_argument(i+1,astr)
         read(astr,*)deflate
      case('--help')
         if (mapl_am_I_root()) then
         
         end if
         call MPI_Finalize(status)
         return
      end select
    enddo

    if (trim(regridMth) .ne. 'bilinear' .and. trim(regridMth ) .ne. 'conservative' .and. trim(regridMth ) .ne. 'conservative2' .and. &
         trim(regridMth).ne.'patch') then
       if (MAPL_AM_I_Root()) write(*,*)'invalid regrid method choose bilinear or conservative'
       _ASSERT(.false.,'needs informative message')
    end if
    if (trim(regridMth) == 'bilinear') then
       regridMethod = REGRID_METHOD_BILINEAR
    end if
    if (trim(regridMth) == 'patch') then
       regridMethod = REGRID_METHOD_PATCH
    end if
    if (trim(regridMth) == 'conservative') then
       regridMethod = REGRID_METHOD_CONSERVE
    end if
    if (trim(regridMth) == 'conservative2') then
       regridMethod = REGRID_METHOD_CONSERVE_2ND
    end if

    inquire(file=trim(outputfile),exist=file_exists)
    _ASSERT(.not.file_exists,"output file already exists: exiting!")

    call MAPL_GetNodeInfo (comm=MPI_COMM_WORLD, rc=status)
    _VERIFY(STATUS)

    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
    _VERIFY(STATUS)

    call formatter%open(trim(filename),pFIO_Read,rc=status)
    _VERIFY(status)
    basic_metadata=formatter%read(rc=status)
    _VERIFY(status)
    call metadata%create(basic_metadata,trim(filename))

    call formatter%close(rc=status)
    _VERIFY(status)

    tsteps = metadata%get_dimension('time',rc=status)
    _VERIFY(status)
    call metadata%get_time_info(timeVector=tSeries,rc=status)
    _VERIFY(status)
    lev_name=metadata%get_level_name()
    if (trim(lev_name)/='') then
       lm_world = metadata%get_dimension(lev_name,rc=status)
       _VERIFY(status)
    end if


    if (.not.allTimes) then
       tSteps=1
       call UnpackDateTIme(itime,year,month,day,hour,minute,second)
       deallocate(tSeries)
       allocate(tSeries(1))
       call ESMF_TimeSet(tSeries(1), yy=year, mm=month, dd=day,  h=hour,  m=minute, s=second,__RC__)
    end if
    if (tSteps == 1) then
       call ESMF_TimeIntervalSet( TimeInterval, h=6, m=0, s=0, rc=status )
       _VERIFY(STATUS)
    else
       TimeInterval=tSeries(2)-tSeries(1)
    end if
    call ESMF_TimeIntervalGet(TimeInterval,h=hour,m=minute,s=second,__RC__)
    tint=hour*10000+minute*100+second
    Clock = ESMF_ClockCreate ( name="Eric", timeStep=TimeInterval, &
                               startTime=tSeries(1), rc=status )
    _VERIFY(STATUS)

    call UnpackGridName(Gridname,im_world_new,jm_world_new,dateline_new,pole_new)

    cfoutput = create_cf(gridname,im_world_new,jm_world_new,nx,ny,lm_world,cs_stretch_param,newcube,__RC__)
    grid_new=grid_manager%make_grid(cfoutput,prefix=trim(gridname)//".",__RC__)

    if (mapl_am_i_root()) write(*,*)'done making grid'

    bundle=ESMF_FieldBundleCreate(name="cfio_bundle",rc=status)
    call ESMF_FieldBundleSet(bundle,grid=grid_new,rc=status)
    _VERIFY(STATUS)

    fileCreated=.false.
    do i=1,tsteps

       
       call t_prof%start("Read")
       if (mapl_am_i_root()) write(*,*)'processing timestep ',i
       time = tSeries(i)
       if (onlyvars) then
          call MAPL_Read_bundle(bundle,trim(filename),time=time,regrid_method=regridMethod,only_vars=vars,rc=status)
       else
          call MAPL_Read_bundle(bundle,trim(filename),time=time,regrid_method=regridMethod,rc=status)
       end if
       _VERIFY(STATUS)
       call t_prof%stop("Read")

       call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
       _VERIFY(STATUS)

       call t_prof%start("write")

       if (mapl_am_I_root()) write(*,*) "moving on to writing the file"

       call ESMF_ClockSet(clock,currtime=time,__RC__)
       if (.not.fileCreated) then
          call newWriter%create_from_bundle(bundle,clock,outputFile,n_steps=tsteps,time_interval=tint,nbits=shave,deflate=deflate,rc=status)
          _VERIFY(status)
          fileCreated=.true.
       end if
       call newWriter%write_to_file(rc=status)
       _VERIFY(status)
       call t_prof%stop("write")
 
    end do

!   All done
!   --------
    call ESMF_VMBarrier(VM,__RC__)

    call io_server%finalize()
    !call o_server%finalize()
    !call i_server%finalize()
    call t_prof%finalize()
    call t_prof%reduce()
    call generate_report()
    call MAPL_Finalize(__RC__)
    call ESMF_Finalize ( rc=status )
    _VERIFY(STATUS)

    end subroutine main

   subroutine UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: DATETIME(:)
     integer, intent(  OUT) :: YY, MM, DD, H, M, S

     YY =     datetime(1)/10000
     MM = mod(datetime(1),10000)/100
     DD = mod(datetime(1),100)
     H  =     datetime(2)/10000
     M  = mod(datetime(2),10000)/100
     S  = mod(datetime(2),100)
     return
   end subroutine UnpackDateTime

   subroutine UnpackGridName(gridName,im,jm,date,pole)
     character(len=*), intent(in) :: gridName
     integer,          intent(out) :: im
     integer,          intent(out) :: jm
     character(len=2), intent(out) :: date
     character(len=2), intent(out) :: pole

     integer :: nn
     character(len=5) :: imsz,jmsz

     nn   = len_trim(Gridname)
     imsz = Gridname(3:index(Gridname,'x')-1)
     jmsz = Gridname(index(Gridname,'x')+1:nn-3)
     pole = Gridname(1:2)
     date = Gridname(nn-1:nn)
     read(IMSZ,*) IM
     read(JMSZ,*) JM

    end subroutine

    subroutine guesspole_and_dateline(lons,lats,date,pole)
     real, intent(in) :: lons(:)
     real, intent(in) :: lats(:)
     character(len=2), intent(out) :: date
     character(len=2), intent(out) :: pole
 
     if (size(lats) == 6*size(lons)) then
        pole='PE'
        date='CF'
        return
     end if

     if (abs(LONS(1)+180._REAL64) .GT. abs(LONS(2)-LONS(1)) ) then
        date='DE'
     else
        date='DC'
     end if
     if ( abs(LATS(1)+90._REAL64) .GT. abs(LATS(2)-LATS(1)) ) then
        pole='PE'
     else
        pole='PC'
     end if
    end subroutine guesspole_and_dateline 

    function create_cf(grid_name,im_world,jm_world,nx,ny,lm,cs_stretch_param,newCube,rc) result(cf)
       use MAPL_ConfigMod
       type(ESMF_Config)              :: cf
       character(len=*), intent(in) :: grid_name
       integer, intent(in)          :: im_world,jm_world
       integer, intent(in)          :: nx,ny
       integer, intent(in)          :: lm
       real, intent(in)             :: cs_stretch_param(3)
       logical, intent(in)          :: newCube
       integer, optional, intent(out) :: rc

       character(len=ESMF_MAXSTR),parameter :: Iam = "create_cf"
       integer :: status
       character(len=2) :: pole,dateline
       integer :: nn

       nn = len_trim(grid_name)
       dateline=grid_name(nn-1:nn)
       pole=grid_name(1:2)

       cf = MAPL_ConfigCreate(__RC__)
       call MAPL_ConfigSetAttribute(cf,value=NX, label=trim(grid_name)//".NX:",rc=status)
       VERIFY_(status)
       call MAPL_ConfigSetAttribute(cf,value=lm, label=trim(grid_name)//".LM:",rc=status)
       VERIFY_(status)
       if (jm_world==6*im_world) then
          if (newCube) then
             call MAPL_ConfigSetAttribute(cf,value="Cubed-Sphere", label=trim(grid_name)//".GRID_TYPE:",rc=status)
             VERIFY_(status)
          else
             call MAPL_ConfigSetAttribute(cf,value="Old-Cubed-Sphere", label=trim(grid_name)//".GRID_TYPE:",rc=status)
             VERIFY_(status)
          end if
          call MAPL_ConfigSetAttribute(cf,value=6, label=trim(grid_name)//".NF:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cf,value=ny/6, label=trim(grid_name)//".NY:",rc=status)
          VERIFY_(status)
          if (any(cs_stretch_param/=cs_stretch_uninit)) then
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(1),label=trim(grid_name)//".STRETCH_FACTOR:",rc=status)
             VERIFY_(status)
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(2),label=trim(grid_name)//".TARGET_LON:",rc=status)
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(3),label=trim(grid_name)//".TARGET_LAT:",rc=status)
          end if
     
       else
          call MAPL_ConfigSetAttribute(cf,value="LatLon", label=trim(grid_name)//".GRID_TYPE:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cf,value=jm_world,label=trim(grid_name)//".JM_WORLD:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cf,value=ny, label=trim(grid_name)//".NY:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cf,value=pole, label=trim(grid_name)//".POLE:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cf,value=dateline, label=trim(grid_name)//".DATELINE:",rc=status)
          VERIFY_(status)
       end if


     end function create_cf 

    function create_gridname(im,jm,date,pole) result(gridname)
     integer, intent(in) :: im
     integer, intent(in) :: jm
     character(len=2), intent(in) :: date
     character(len=2), intent(in) :: pole
     character(len=ESMF_MAXSTR) :: gridname
     character(len=16) :: imstr,jmstr
     write(imstr,*) im
     write(jmstr,*) jm
     gridname =  pole // trim(adjustl(imstr))//'x'//&
                 trim(adjustl(jmstr))//'-'//date

    end function create_gridname

    subroutine generate_report()

         character(:), allocatable :: report_lines(:)
         integer :: i
         character(1) :: empty(0)
   
         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(20))
         call reporter%add_column(FormattedTextColumn('Inclusive','(f9.6)', 9, InclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Incl','(f6.2)', 6, PercentageColumn(InclusiveColumn('MEAN'),'MAX')))
         call reporter%add_column(FormattedTextColumn('Exclusive','(f9.6)', 9, ExclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Excl','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MEAN'))))   
         call reporter%add_column(FormattedTextColumn(' Max Excl)','(f9.6)', 9, ExclusiveColumn('MAX')))
         call reporter%add_column(FormattedTextColumn(' Min Excl)','(f9.6)', 9, ExclusiveColumn('MIN')))
         call reporter%add_column(FormattedTextColumn('Max PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MAX_PE')))
         call reporter%add_column(FormattedTextColumn('Min PE)','(1x,i4.4,1x)', 6, ExclusiveColumn('MIN_PE'))) 
        report_lines = reporter%generate_report(t_prof)
         if (mapl_am_I_root()) then 
            write(*,'(a)')'Final profile'
            write(*,'(a)')'============='
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
            end do
            write(*,'(a)') ''
         end if
    end subroutine generate_report
 
    end program ut_ReGridding 

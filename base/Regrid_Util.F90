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

 
   implicit NONE

   class(AbstractRegridder), pointer :: regridder_esmf=>null()
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
   type(ESMF_Grid)     :: grid_old, grid_new
   type(ESMF_VM)       :: vm             ! ESMF Virtual Machine

   character(len=ESMF_MAXPATHLEN) ::  Filename,OutputFile,tp_filein,tp_fileout

!  The CFIO object associated with a disk file
!  -------------------------------------------
   type(MAPL_CFIO) :: cfio_esmf

!  Basic information about the parallel environment
!         PET = Persistent Execution Threads
!  In the current implementation, a PET is equivalent 
!  to an MPI process
!  ------------------------------------------------
   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc
   integer :: Nx,Ny,nargs
   integer :: IM_World0,JM_World0,LM_World
   integer :: IM_World_new, JM_World_new
   type(ESMF_CFIO) :: lcfio
   type(ESMF_CFIOGrid), pointer :: CFIOGRID 

   type(ESMF_FieldBundle) :: bundle_cfio, bundle_esmf
   type(ESMF_Time) :: time
   type(ESMF_Time), allocatable :: tSeries(:)
   type(ESMF_TimeInterval) :: timeInterval
   type(ESMF_Clock) :: clock

   logical :: fileCreated

   character(len=ESMF_MAXSTR) :: RegridMth

   character(len=ESMF_MAXSTR) :: Iam

   integer :: second,minute,hour,day,month,year,itime(2),begDate,begTime,tsteps,i,nymdB, nhmsB,freq

   character(len=2) :: pole_new,dateline_new
   character(len=2) :: pole_old,dateline_old
   logical :: onlyVars, allTimes, newCube
   character(len=512) :: vars
   integer(ESMF_KIND_I8), allocatable :: tseriesInt(:)
   integer(ESMF_KIND_I8) :: iCurrInterval
   character(len=ESMF_MAXSTR) :: gridname
   character(len=ESMF_MAXPATHLEN) :: str,astr
   real, pointer :: lonsfile(:), latsfile(:)
   character(len=:), allocatable :: tripolar_file_in,tripolar_file_out,old_gridname
   type(ESMF_CONFIG) :: cfinput,cfoutput
   integer :: regridMethod
   real :: cs_stretch_param(3)
   integer :: deflate, shave
 
    Iam = "ut_ReGridding"

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF's automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_NONE, vm=vm, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet(vm, localPET=myPET, petCount=nPet)
    call MAPL_Initialize(__RC__)
    t_prof=DistributedProfiler('Regrid_Util',MpiTimerGauge(),MPI_COMM_WORLD)
 
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

    call MAPL_GetNodeInfo (comm=MPI_COMM_WORLD, rc=status)
    _VERIFY(STATUS)

    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
    _VERIFY(STATUS)
    call ESMF_CFIOSet(lcfio,fname=trim(filename),__RC__)
    call ESMF_CFIOFileOpen(lcfio,FMODE=1,__RC__)
    call ESMF_CFIOGet       (LCFIO,     grid=CFIOGRID, __RC__)
    call ESMF_CFIOGridGet   (CFIOGRID, IM=IM_WORLD0, JM=JM_WORLD0, KM=LM_WORLD, Lon=lonsfile,lat=latsfile, __RC__)
    call guesspole_and_dateline(lonsfile,latsfile,dateline_old,pole_old)
    old_gridname = create_gridname(im_world0,jm_world0,dateline_old,pole_old)
    if (.not.allTimes) then
       tSteps=1
       call UnpackDateTIme(itime,year,month,day,hour,minute,second)
       allocate(tSeries(1))
       call ESMF_TimeSet(tSeries(1), yy=year, mm=month, dd=day,  h=hour,  m=minute, s=second,__RC__)
    else
       tSteps=lcfio%tSteps
       allocate(tSeriesInt(tSteps))
       call getDateTimeVec(lcfio%fid,begDate,begTime,tSeriesInt,__RC__)
       allocate(tSeries(tSteps))
       do i=1,tSteps
           iCurrInterval = tSeriesInt(i)
           call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
           call MAPL_UnpackTime(nymdB,year,month,day)
           call MAPL_UnpackTime(nhmsB,hour,minute,second)
           call ESMF_TimeSet(tSeries(i), yy=year, mm=month, dd=day,  h=hour,  m=minute, s=second,__RC__)
       enddo
       icurrInterval = tSeriesInt(1)
       call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
       call MAPL_UnpackTime(nymdB,year,month,day)
       call MAPL_UnpackTime(nhmsB,hour,minute,second)
    end if
    call ESMF_CFIOFileClose(lcfio)

    if (tSteps == 1) then
       call ESMF_TimeIntervalSet( TimeInterval, h=6, m=0, s=0, rc=status )
       _VERIFY(STATUS)
    else
       TimeInterval=tSeries(2)-tSeries(1)
    end if
    Clock = ESMF_ClockCreate ( name="Eric", timeStep=TimeInterval, &
                               startTime=tSeries(1), rc=status )
    _VERIFY(STATUS)

    call UnpackGridName(Gridname,im_world_new,jm_world_new,dateline_new,pole_new)

    if (mapl_am_i_root()) write(*,*)'going to make grid',im_world0,jm_world0,im_world_new,jm_world_new
    cfinput = create_cf(old_gridname,im_world0,jm_world0,nx,ny,lm_world,cs_stretch_param,__RC__)
    cfoutput = create_cf(gridname,im_world_new,jm_world_new,nx,ny,lm_world,cs_stretch_param,__RC__)
    grid_old=grid_manager%make_grid(cfinput,prefix=trim(old_gridname)//".",__RC__)
    grid_new=grid_manager%make_grid(cfoutput,prefix=trim(gridname)//".",__RC__)
    call t_prof%start("GenRegrid")
    regridder_esmf => new_regridder_manager%make_regridder(grid_old,grid_new,regridMethod,__RC__)
    call t_prof%stop("GenRegrid")

    if (mapl_am_i_root()) write(*,*)'done making grid'

    bundle_cfio=ESMF_FieldBundleCreate(name="cfio_bundle",rc=status)
    _VERIFY(STATUS)
    call ESMF_FieldBundleSet(bundle_cfio,grid=grid_old,rc=status)
    _VERIFY(STATUS)

    fileCreated=.false.
    do i=1,tsteps

       
       call t_prof%start("Read")
       if (mapl_am_i_root()) write(*,*)'processing timestep ',i
       time = tSeries(i)
       if (onlyvars) then
          call MAPL_CFIORead(filename,time,bundle_cfio,only_vars=vars,rc=status)
       else
          call MAPL_CFIORead(filename,time,bundle_cfio,rc=status)
       end if
       _VERIFY(STATUS)

       if (Mapl_AM_I_Root()) write(*,*)'done reading file ',trim(filename)

       if (.not.fileCreated) bundle_esmf = BundleClone(bundle_cfio,grid_new,__RC__)
       call t_prof%stop("Read")

       call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
       _VERIFY(STATUS)
       _VERIFY(STATUS)

       call t_prof%start("regrid")
       call RunESMFRegridding(bundle_cfio,bundle_esmf,shave,__RC__)
       call t_prof%stop("regrid")

       call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
       _VERIFY(STATUS)
 
       call t_prof%start("write")


       if (mapl_am_I_root()) write(*,*) "moving on to writing the file"

       call ESMF_ClockSet(clock,currtime=time,__RC__)
       if (.not.fileCreated) then
          call ESMF_TimeIntervalGet(timeInterval,s=freq,__RC__)
          call MAPL_CFIOCreate ( cfio_esmf, outputFile, clock, Bundle_esmf,frequency=freq,vunit = "layer", deflate=deflate, rc=status )
          _VERIFY(STATUS)
          call MAPL_CFIOSet(cfio_esmf,newFormat=newCube,rc=status)
          _VERIFY(STATUS)
       end if
       call MAPL_CFIOWrite(cfio_esmf,clock,bundle_esmf,created=fileCreated,rc=status)
       _VERIFY(STATUS)
       if (.not.fileCreated) fileCreated=.true.
       call t_prof%stop("write")
 
    end do
    call MAPL_CFIOClose(cfio_esmf,rc=status)
    _VERIFY(STATUS)

!   All done
!   --------
    call ESMF_VMBarrier(VM,__RC__)

    call t_prof%finalize()
    call t_prof%reduce()
    call generate_report()
    call MAPL_Finalize(__RC__)
    call MPI_Finalize(status)
!    call ESMF_Finalize ( rc=status )
!    _VERIFY(STATUS)

    end subroutine main


    function BundleClone(Bundle_old,grid_new,rc) result(Bundle_new)

    type(ESMF_FieldBundle), intent(inout) :: Bundle_old
    type(ESMF_Grid),        intent(inout) :: grid_new
 
    type(ESMF_FieldBundle)                :: Bundle_new
    integer, optional,      intent(out  ) :: rc

    integer :: status
    character(LEN=ESMF_MAXSTR) :: Iam

    integer :: bcount, i
    type(ESMF_Field) :: field
    type(ESMF_Field) :: field_old
    character(LEN=ESMF_MAXSTR) :: FieldName
    integer :: dims_orig

    Iam = "BundleClone"        


    call ESMF_FieldBundleGet(bundle_old,fieldCount=bcount,rc=status)
    _VERIFY(STATUS)

    bundle_new = ESMF_FieldBundleCreate(name="newBundle",rc=status)
    _VERIFY(STATUS)
    call ESMF_FieldBundleSet(bundle_new,grid=grid_new,__RC__)

    do i=1,bcount
       ! get info about original fields in bundle
       call ESMF_FieldBundleGet(bundle_old,i,field=field_old,rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field_old,name=FieldName,rc=status)
       _VERIFY(STATUS)

       call ESMF_AttributeGet(field_old,NAME='DIMS',value=dims_orig,rc=status)
       _VERIFY(STATUS)
       field = MAPL_FieldCreate(field_old,grid_new,rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeSet(field,NAME='DIMS',value=dims_orig,rc=status)
       _VERIFY(STATUS)
       call MAPL_FieldBundleAdd(bundle_new,field,__RC__)

    end do

    _RETURN(ESMF_SUCCESS)

    end function BundleClone

    subroutine BundleCopy(Bundle_old,Bundle_new,rc)

    type(ESMF_FieldBundle), intent(inout) :: Bundle_old
    type(ESMF_FieldBundle), intent(inout) :: Bundle_new
 
    integer, optional,      intent(out  ) :: rc

    integer :: status
    character(LEN=ESMF_MAXSTR) :: Iam

    integer :: bcount, i
    type(ESMF_Field) :: field_new
    type(ESMF_Field) :: field_old
    real, pointer, dimension(:,:) :: ptr2d_old => null()
    real, pointer, dimension(:,:) :: ptr2d_new => null()
    real, pointer, dimension(:,:,:) :: ptr3d_old => null()
    real, pointer, dimension(:,:,:) :: ptr3d_new => null()
    integer :: dims, vloc

    Iam = "BundleCopy"        


    call ESMF_FieldBundleGet(bundle_old,fieldCount=bcount,rc=status)
    _VERIFY(STATUS)


    do i=1,bcount
       ! get info about original fields in bundle
       call ESMF_FieldBundleGet(bundle_old,i,field=field_old,rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(field_old,NAME='DIMS',value=dims,rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(field_old,NAME='VLOCATION',value=vloc,rc=status)
       _VERIFY(STATUS)

       call ESMF_FieldBundleGet(bundle_new,i,field=field_new,rc=status)
       _VERIFY(STATUS)
      
       if (dims==MAPL_DimsHorzOnly) then
          call ESMF_FieldGet(field_old,0,ptr2d_old,__RC__)
          call ESMF_FieldGet(field_new,0,ptr2d_new,__RC__)
          ptr2d_new=ptr2d_old
       else if (dims==MAPL_DimsHorzVert) then
          call ESMF_FieldGet(field_old,0,ptr3d_old,__RC__)
          call ESMF_FieldGet(field_new,0,ptr3d_new,__RC__)
          ptr3d_new=ptr3d_old
       end if

    end do

    _RETURN(ESMF_SUCCESS)

    end subroutine BundleCopy

    subroutine RunESMFRegridding(bundle_old,bundle_new,shave,rc)

    type(ESMF_FieldBundle),     intent(inout) :: bundle_old
    type(ESMF_FieldBundle),     intent(inout) :: bundle_new
    integer, intent(in) :: shave

    integer, optional,      intent(out  ) :: rc

    character(len=ESMF_MAXSTR) :: Iam
    integer :: status

    type(ESMF_Field) :: field_old, field_new
    integer :: bcount,i
    real(ESMF_KIND_R4), pointer  :: ptr3d_old(:,:,:) => null()
    real(ESMF_KIND_R4), pointer  :: ptr3d_new(:,:,:) => null()
    real(ESMF_KIND_R4), pointer  :: ptr2d_old(:,:) => null()
    real(ESMF_KIND_R4), pointer  :: ptr2d_new(:,:) => null()
    integer                 :: oldRank, newRank

    Iam = "RunESMFRegridding"

   call ESMF_FieldBundleGet(bundle_old,fieldcount=bcount,__RC__)
   do i=1,bcount

       call ESMF_FieldBundleGet(bundle_old,i,field=field_old,__RC__)
       call ESMF_FieldGet(field_old,rank=oldRank,__RC__)

       call ESMF_FieldBundleGet(bundle_new,i,field=field_new,__RC__)
       call ESMF_FieldGet(field_new,rank=newRank,__RC__)

       _ASSERT(newRank == oldRank,'needs informative message')

       if (oldRank == 3) then

          call ESMF_FieldGet(field_old,0,farrayPtr=ptr3D_old,__RC__)
          call ESMF_FieldGet(field_new,0,farrayPtr=ptr3D_new,__RC__)
          call regridder_esmf%regrid(ptr3d_old,ptr3d_new,__RC__)
          if (shave < 24) then
             call pFIO_DownBit(ptr3d_new,ptr3d_new,shave,undef=MAPL_undef,rc=status)
             _VERIFY(status)
          end if
          nullify(ptr3d_old)
          nullify(ptr3d_new)

       else if (oldRank == 2) then

          call ESMF_FieldGet(field_old,0,farrayPtr=ptr2D_old,__RC__)
          call ESMF_FieldGet(field_new,0,farrayPtr=ptr2D_new,__RC__)
          call regridder_esmf%regrid(ptr2d_old,ptr2d_new,__RC__)
          if (shave < 24) then
             call pFIO_DownBit(ptr2d_new,ptr2d_new,shave,undef=MAPL_undef,rc=status)
             _VERIFY(status)
          end if
          nullify(ptr2d_old)
          nullify(ptr2d_new)
       end if
    end do

    _RETURN(ESMF_SUCCESS)

    end subroutine RunESMFRegridding

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

    function create_cf(grid_name,im_world,jm_world,nx,ny,lm,cs_stretch_param,rc) result(cf)
       use MAPL_ConfigMod
       type(ESMF_Config)              :: cf
       character(len=*), intent(in) :: grid_name
       integer, intent(in)          :: im_world,jm_world
       integer, intent(in)          :: nx,ny
       integer, intent(in)          :: lm
       real, intent(in)             :: cs_stretch_param(3)
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
          call MAPL_ConfigSetAttribute(cf,value="Cubed-Sphere", label=trim(grid_name)//".GRID_TYPE:",rc=status)
          VERIFY_(status)
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

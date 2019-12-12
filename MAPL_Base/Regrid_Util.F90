#include "MAPL_Generic.h"

   Program ut_ReGridding

   use ESMF
   use ESMFL_Mod
   use MAPL_BaseMod
   use MAPL_MemUtilsMod
   use MAPL_CFIOMod
   use MAPL_CommsMod
   use MAPL_ShmemMod
   use ESMF_CFIOMod
   use ESMF_CFIOUtilMod
   use ESMF_CFIOFileMod
   use MAPL_LatLonGridFactoryMod
   use MAPL_ConstantsMod, only: MAPL_PI_R8
   use MAPL_ErrorHandlingMod

   implicit NONE

   type(ESMF_RouteHandle) :: regrid_rh
   type(ESMF_DynamicMask) :: dynamicMask
   logical :: createdHandle

   integer, parameter :: grid_ll = 1
   integer, parameter :: grid_cs = 2
   integer, parameter :: grid_tp = 3

   include "mpif.h"

   call main()
    
CONTAINS
 
    subroutine main()

!CONTAINS

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)     :: grid_old, grid_new
   type(ESMF_VM)       :: vm             ! ESMF Virtual Machine

   real(kind=ESMF_KIND_R8) :: itime_end, itime_beg

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
   integer :: inGrid, outGrid
 
    Iam = "ut_ReGridding"

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF's automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_NONE, vm=vm, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet(vm, localPET=myPET, petCount=nPet)


    createdHandle=.false.
    nx=1
    ny=6
    onlyvars=.false.
    alltimes=.true.
    regridMth='bilinear'
    newCube=.true.
    inGrid = grid_ll
    outGrid = grid_ll
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
      case('-method')
         call get_command_argument(i+1,RegridMth)
      case('-cubeFormat')
         call get_command_argument(i+1,astr)
         if (trim(astr) == 'new') newCube=.true.
         if (trim(astr) == 'old') newCube=.false.
      case('-tp_in')
         call get_command_argument(i+1,tp_filein)
         inGrid=grid_tp
      case('-tp_out')
         call get_command_argument(i+1,tp_fileout)
         outGrid=grid_tp
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

    call MAPL_GetNodeInfo (comm=MPI_COMM_WORLD, rc=status)
    _VERIFY(STATUS)

    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
    _VERIFY(STATUS)
    if (.not.allTimes) then
       call UnpackDateTIme(itime,year,month,day,hour,minute,second)
    end if
    call ESMF_CFIOSet(lcfio,fname=trim(filename),__RC__)
    call ESMF_CFIOFileOpen(lcfio,FMODE=1,__RC__)
    call ESMF_CFIOGet       (LCFIO,     grid=CFIOGRID, __RC__)
    call ESMF_CFIOGridGet   (CFIOGRID, IM=IM_WORLD0, JM=JM_WORLD0, KM=LM_WORLD, Lon=lonsfile,lat=latsfile, __RC__)
    call guesspole_and_dateline(lonsfile,latsfile,dateline_old,pole_old)
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
!   Lat lon or cubed sphere?
!   ------------------------
    if ( JM_World0 == 6*IM_World0 ) then
       inGrid = grid_cs
    end if

    if ( JM_World_new == 6*IM_World_new ) then
       outGrid = grid_cs
    end if


    if (mapl_am_i_root()) write(*,*)'going to make grid',im_world0,jm_world0,im_world_new,jm_world_new
    grid_old = create_grid(inGrid,"unknown",im_world0,jm_world0,lm_world,nx,ny,dateline_old,pole_old,tp_filein,__RC__)
    grid_new = create_grid(outGrid,gridname,im_world_new,jm_world_new,lm_world,nx,ny,dateline_new,pole_new,tp_fileout,__RC__)
    if (mapl_am_i_root()) write(*,*)'done making grid'

    bundle_cfio=ESMF_FieldBundleCreate(name="cfio_bundle",rc=status)
    _VERIFY(STATUS)
    call ESMF_FieldBundleSet(bundle_cfio,grid=grid_old,rc=status)
    _VERIFY(STATUS)

    fileCreated=.false.
    do i=1,tsteps

       
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

       call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
       _VERIFY(STATUS)
       itime_beg = MPI_Wtime(STATUS)
       _VERIFY(STATUS)

       call RunESMFRegridding(regridMth,bundle_cfio,bundle_esmf,__RC__)

       call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
       _VERIFY(STATUS)
       itime_end = MPI_Wtime(STATUS)
       if (mapl_am_I_root()) write(*,*)'MAPL TIME: ',itime_end-itime_beg


       if (mapl_am_I_root()) write(*,*) "moving on to writing the file"

       call ESMF_ClockSet(clock,currtime=time,__RC__)
       if (.not.fileCreated) then
          call ESMF_TimeIntervalGet(timeInterval,s=freq,__RC__)
          call MAPL_CFIOCreate ( cfio_esmf, outputFile, clock, Bundle_esmf,frequency=freq,vunit = "layer", rc=status )
          _VERIFY(STATUS)
          call MAPL_CFIOSet(cfio_esmf,newFormat=newCube,rc=status)
          _VERIFY(STATUS)
       end if
       call MAPL_CFIOWrite(cfio_esmf,clock,bundle_esmf,created=fileCreated,rc=status)
       _VERIFY(STATUS)
       if (.not.fileCreated) fileCreated=.true.
 
    end do
    call MAPL_CFIOClose(cfio_esmf,rc=status)
    _VERIFY(STATUS)

!   All done
!   --------
    call ESMF_VMBarrier(VM,__RC__)

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

    subroutine RunESMFRegridding(regridMth,bundle_old,bundle_new,rc)

    character(len=*),           intent(in   ) :: regridMth
    type(ESMF_FieldBundle),     intent(inout) :: bundle_old
    type(ESMF_FieldBundle),     intent(inout) :: bundle_new

    integer, optional,      intent(out  ) :: rc

    character(len=ESMF_MAXSTR) :: Iam
    integer :: status

    type(ESMF_Field) :: field_old, field_new, srcField,dstField
    type(ESMF_Grid) :: grid_old, grid_new
    integer :: bcount,i,l
    real(ESMF_KIND_R4), pointer  :: ptr3d_old(:,:,:) => null()
    real(ESMF_KIND_R4), pointer  :: ptr3d_new(:,:,:) => null()
    real(ESMF_KIND_R4), pointer  :: srcPtr(:,:) => null()
    real(ESMF_KIND_R4), pointer  :: dstPtr(:,:) => null()
    type(ESMF_RegridMethod_Flag) :: regridMethod
    type(ESMF_PoleMethod_Flag)   :: PoleMethod
    integer                 :: oldRank, newRank,srcterm
    real(kind=ESMF_KIND_R4), pointer :: ptr2d(:,:)

    Iam = "RunESMFRegridding"

    if (trim(regridMth) == 'bilinear') then
       regridMethod = ESMF_REGRIDMETHOD_BILINEAR
       PoleMethod = ESMF_POLEMETHOD_TEETH
    end if
    if (trim(regridMth) == 'patch') then
       regridMethod = ESMF_REGRIDMETHOD_PATCH
       PoleMethod = ESMF_POLEMETHOD_TEETH
    end if
    if (trim(regridMth) == 'conservative') then
       regridMethod = ESMF_REGRIDMETHOD_CONSERVE
       PoleMethod = ESMF_POLEMETHOD_NONE
    end if
    if (trim(regridMth) == 'conservative2') then
       regridMethod = ESMF_REGRIDMETHOD_CONSERVE_2ND
       PoleMethod = ESMF_POLEMETHOD_NONE
    end if

    call ESMF_FieldBundleGet(bundle_old,grid=grid_old,__RC__)
    call ESMF_FieldBundleGet(bundle_new,grid=grid_new,__RC__)

    srcField = MAPL_FieldCreateEmpty(name="srcField",grid=grid_old,__RC__)
    call MAPL_FieldAlloccommit(srcField,dims=MAPL_DimsHorzOnly,location=MAPL_VLocationNone, &
        typekind=kind(0.0),hw=0,__RC__)
    call ESMF_FieldGet(srcField,localDE=0,farrayPtr=ptr2d,__RC__)
    ptr2d = 0.0

    dstField = MAPL_FieldCreateEmpty(name="dstField",grid=grid_new,__RC__)
    call MAPL_FieldAlloccommit(dstField,dims=MAPL_DimsHorzOnly,location=MAPL_VLocationNone, &
        typekind=kind(0.0),hw=0,__RC__)
    call ESMF_FieldGet(dstField,localDE=0,farrayPtr=ptr2d,__RC__)
    ptr2d = 0.0

    srcTerm=0
    if (.not.createdHandle) then
       call ESMF_FieldRegridStore(srcField, dstField, &
              & regridmethod=regridMethod, lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
              & srcTermProcessing=srcTerm, &
              & srcMaskValues = [0], &
              & unmappedAction=ESMF_UNMAPPEDACTION_IGNORE, &
              & routehandle=regrid_rh, __RC__)
       createdHandle=.true.
       call ESMF_DynamicMaskSetR4R8R4(dynamicMask,simpleDynMaskProc,dynamicSrcMaskValue=MAPL_UNDEF,__RC__)
    end if

    call ESMF_FieldBundleGet(bundle_old,fieldcount=bcount,__RC__)
   do i=1,bcount

       call ESMF_FieldBundleGet(bundle_old,i,field=field_old,__RC__)
       call ESMF_FieldGet(field_old,rank=oldRank,__RC__)

       call ESMF_FieldBundleGet(bundle_new,i,field=field_new,__RC__)
       call ESMF_FieldGet(field_new,rank=newRank,__RC__)

       _ASSERT(newRank == oldRank,'needs informative message')

       if (oldRank == 3) then

          call ESMF_FieldGet(field_old,0,farrayPtr=ptr3D_old,__RC__)
          call ESMF_FieldGet(srcfield,0,farrayPtr=srcPtr,__RC__)
          call ESMF_FieldGet(field_new,0,farrayPtr=ptr3D_new,__RC__)
          call ESMF_FieldGet(dstfield,0,farrayPtr=dstPtr,__RC__)
          do l=1,size(ptr3D_old,3)

             srcPtr = ptr3d_old(:,:,l)
             call ESMF_FieldRegrid(srcField,dstField,routehandle=regrid_rh, &
                  termorderflag=ESMF_TERMORDER_SRCSEQ, dynamicMask=dynamicMask,__RC__)
             ptr3d_new(:,:,l) = dstPtr

          end do

          nullify(ptr3d_old)
          nullify(ptr3d_new)
          nullify(srcPtr)
          nullify(dstPtr)

       else if (oldRank == 2) then

          call ESMF_FieldRegrid(field_old,field_new,routehandle=regrid_rh, &
               termorderflag=ESMF_TERMORDER_SRCSEQ, &
               zeroregion=ESMF_REGION_SELECT, dynamicMask=dynamicMask, __RC__)

       end if
    end do

    _RETURN(ESMF_SUCCESS)

    end subroutine RunESMFRegridding

   subroutine simpleDynMaskProc(dynamicMaskList, dynamicSrcMaskValue, &
      dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4), pointer        :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j
      real(ESMF_KIND_R8)  :: renorm
      if (associated(dynamicMaskList)) then
        do i=1, size(dynamicMaskList)
          dynamicMaskList(i)%dstElement = 0.d0 ! set to zero
          renorm = 0.d0 ! reset
          do j=1, size(dynamicMaskList(i)%factor)
            !if (.not. &
              !match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j))) then
              !dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement &
                !+ dynamicMaskList(i)%factor(j) &
                !* dynamicMaskList(i)%srcElement(j)
              !renorm = renorm + dynamicMaskList(i)%factor(j)
            !endif
            if (dynamicSrcMaskValue /= dynamicMaskList(i)%srcElement(j)) then
              dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement &
                + dynamicMaskList(i)%factor(j) &
                * dynamicMaskList(i)%srcElement(j)
              renorm = renorm + dynamicMaskList(i)%factor(j)
            endif
          enddo
          if (renorm > 0.d0) then
            dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
          else if (present(dynamicSrcMaskValue)) then
            dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
          else
            rc = ESMF_RC_ARG_BAD  ! error detected
            return
          endif
        enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
    end subroutine

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
 
    function cs_gridcreate(grid_name,im_world,nx,lm,rc) result(grid)
       type(ESMF_Grid)              :: grid
       character(len=*), intent(in) :: grid_name
       integer, intent(in)          :: im_world
       integer, intent(in)          :: nx
       integer, intent(in)          :: lm
       integer, optional, intent(out) :: rc

       character(len=*),parameter :: Iam = "cs_gridcreate"
       integer :: i
       integer :: ijms(2,6)
       integer :: status

       ijms(1,1)=nx
       ijms(2,1)=nx
       do i=2,6
          ijms(:,i)=ijms(:,1)
       enddo
       grid = ESMF_GridCreateCubedSPhere(im_world,regDecompPTile=ijms,name=grid_name, &
                staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      _VERIFY(status)

      call ESMF_AttributeSet(grid, name='GRID_LM', value=lm, rc=status)
      _VERIFY(status)

      call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
      _VERIFY(status)
      call ESMF_AttributeSet(grid, name='NEW_CUBE', value=1,rc=status)
      _VERIFY(status)
      _RETURN(ESMF_SUCCESS)

     end function cs_gridcreate
 
    function tripolar_gridcreate(grid_name,im_world,jm_world,nx,ny,lm,gridspec,rc) result(grid)
       type(ESMF_Grid)              :: grid
       character(len=*), intent(in) :: grid_name
       integer, intent(in)          :: im_world,jm_world
       integer, intent(in)          :: nx,ny
       integer, intent(in)          :: lm
       character(len=*), intent(in) :: gridspec
       integer, optional, intent(out) :: rc

       character(len=*),parameter :: Iam = "cs_gridcreate"
       integer :: status

       integer, allocatable :: ims(:),jms(:)
       integer :: i_1,i_n,j_1,j_n, ncid, varid, ic, jc
       real, allocatable :: centers(:,:), corners(:,:,:)
       real(ESMF_KIND_R8), pointer :: fptr(:,:)

       allocate(ims(0:nx-1),jms(0:ny-1))
       call MAPL_DecomposeDim(im_world,ims,nx)
       call MAPL_DecomposeDim(jm_world,jms,ny)

       grid = ESMF_GridCreate1PeriDim( &
            name=trim(grid_name) ,&
            countsPerDEDim1=ims, &
            countsPerDEDim2=jms, &
            indexFlag=ESMF_INDEX_DELOCAL, &
            gridEdgeLWidth=[0,0], &
            gridEdgeUWidth=[0,1], &
            coordDep1=[1,2], &
            coordDep2=[1,2], &
            poleKindFlag=[ESMF_POLEKIND_MONOPOLE,ESMF_POLEKIND_BIPOLE], &
            coordSys=ESMF_COORDSYS_SPH_RAD, &
            __RC__)

       deallocate(ims,jms)

       call ESMF_GridAddCoord(grid, __RC__)
       call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER,__RC__)
       call MAPL_Grid_Interior(grid, i_1, i_n, j_1, j_n)
       status = nf90_open(gridspec,NF90_NOWRITE,ncid)
       _VERIFY(status)

       allocate(centers(im_world,jm_world),__STAT__)

       ! do longitudes
       status = nf90_inq_varid(ncid,'x_T',varid)
       _VERIFY(status)
       status = nf90_get_var(ncid,varid,centers)
       _VERIFY(status)
       centers=centers*MAPL_PI_R8/180.d0
       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)
       ! do latitudes
       status = nf90_inq_varid(ncid,'y_T',varid)
       _VERIFY(status)
       status = nf90_get_var(ncid,varid,centers)
       _VERIFY(status)
       centers=centers*MAPL_PI_R8/180.d0
       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)
       deallocate(centers)
       ! now repeat for corners
       allocate(corners(im_world,jm_world,4),__STAT__)

       ! do longitudes
       status = nf90_inq_varid(ncid,'x_vert_T',varid)
       _VERIFY(status)
       status = nf90_get_var(ncid,varid,corners)
       _VERIFY(status)
       corners=corners*MAPL_PI_R8/180.d0
       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CORNER, &
          farrayPtr=fptr, __RC__)
       ic = size(fptr,1)
       jc = size(fptr,2)
       if (j_n == jm_world) then
          fptr(:,1:jc-1)=corners(i_1:i_n,j_1:j_n,1)
          fptr(:,jc)=corners(i_1:i_n,j_n,4)
       else
          fptr=corners(i_1:i_n,j_1:j_n,1)
       end if
       ! do latitudes
       status = nf90_inq_varid(ncid,'y_vert_T',varid)
       _VERIFY(status)
       status = nf90_get_var(ncid,varid,corners)
       _VERIFY(status)
       corners=corners*MAPL_PI_R8/180.d0
       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CORNER, &
          farrayPtr=fptr, __RC__)
       if (j_n == jm_world) then
          fptr(:,1:jc-1)=corners(i_1:i_n,j_1:j_n,1)
          fptr(:,jc)=corners(i_1:i_n,j_n,4)
       else
          fptr=corners(i_1:i_n,j_1:j_n,1)
       end if
       deallocate(corners)

       call ESMF_AttributeSet(grid,name='GRID_LM',value=lm,__RC__)

       _RETURN(ESMF_SUCCESS)

     end function tripolar_gridcreate 

    function create_grid(grid_type,gname,im_world,jm_world,lm,nx,ny,dateline,pole,tp_file,rc) result(grid)
       type(ESMF_Grid) :: grid
       integer, intent(in) :: grid_type
       character(len=*), intent(in) :: gname
       integer, intent(in) :: im_world,jm_world,lm,nx,ny
       character(len=2), intent(in) :: dateline,pole
       character(len=*), intent(in) :: tp_file
       integer, optional, intent(out) :: rc

       integer :: status
       character(len=ESMF_MAXSTR) :: Iam = "create_grid"
       type(LatLonGridFactory) :: ll_factory

       select case(grid_type)
          case(grid_ll)
             ll_factory = LatLonGridFactory(grid_name=gname,im_world=im_world,jm_world=jm_world,lm=lm, &
                                            nx=nx,ny=ny,pole=pole,dateline=dateline,__RC__)
             grid=ll_factory%make_grid(__RC__)
          case(grid_cs)
             grid = cs_gridcreate(gname,im_world,nx,lm,__RC__)
          case(grid_tp)
             grid = tripolar_gridcreate(gname,im_world,jm_world,nx,ny,lm,tp_file,__RC__)
       end select
       
       _RETURN(ESMF_SUCCESS)
    end function create_grid
        
    end program ut_ReGridding 

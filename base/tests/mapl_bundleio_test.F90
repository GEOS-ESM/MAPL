#include "MAPL_Generic.h"

   module BundleTestSupport

   use mpi
   use ESMF
   use ESMFL_Mod
   use MAPL_Profiler
   use MAPL_BaseMod
   use MAPL_MemUtilsMod
   use MAPL_CommsMod
   use MAPL_ShmemMod
   use MAPL_GridManagerMod
   use MAPL_ExceptionHandling
   use MAPL_ApplicationSupport
   use pFIO
   use MAPL_ESMFFieldBundleWrite
   use MAPL_ESMFFieldBundleRead
   use MAPL_ServerManager
   use MAPL_FileMetadataUtilsMod

   implicit NONE

   real, parameter :: cs_stretch_uninit = -1.0

CONTAINS

   subroutine compare_bundle(State1,State2,tol,rc)
      type(ESMF_FieldBundle), intent(inout) :: State1
      type(ESMF_FieldBundle), intent(inout) :: State2
      real, intent(in)                :: tol
      integer, optional, intent(out) :: rc

      integer :: status
      integer                             :: ii,i,j,k
      real, pointer                       :: ptr3_1(:,:,:) => null()
      real, pointer                       :: ptr3_2(:,:,:) => null()
      real, pointer                       :: ptr2_1(:,:) => null()
      real, pointer                       :: ptr2_2(:,:) => null()
      integer :: itemcount,rank1,rank2,lb(3),ub(3)
      character(len=ESMF_MAXSTR), allocatable :: NameList(:)
      logical, allocatable :: foundDiff(:)
      type(ESMF_Field) :: Field1,Field2

      call ESMF_FieldBundleGet(State1,fieldcount=itemCount,_rc)
         allocate(NameList(itemCount),_stat)
         allocate(foundDiff(itemCount),_stat)
         call ESMF_FieldBundleGet(State1,fieldNameList=NameList,_rc)
         do ii=1,itemCount
            call ESMF_FieldBundleGet(State1,trim(nameList(ii)),field=field1,_rc)
            call ESMF_FieldBundleGet(State2,trim(nameList(ii)),field=field2,_rc)
            call ESMF_FieldGet(field1,rank=rank1,_rc)
            call ESMF_FieldGet(field1,rank=rank2,_rc)
            _assert(rank1==rank2,'needs informative message')
            foundDiff(ii)=.false.
            if (rank1==2) then
               call ESMF_FieldGet(field1,farrayPtr=ptr2_1,_rc)
               call ESMF_FieldGet(field2,farrayPtr=ptr2_2,_rc)
               do i=1,size(ptr2_1,1)
                  do j=1,size(ptr2_1,2)
                     if (abs(ptr2_1(i,j)-ptr2_2(i,j)) .gt. tol) then
                        foundDiff(ii)=.true.
                        exit
                     end if
                  enddo
               enddo
            else if (rank1==3) then
               call ESMF_FieldGet(field1,farrayPtr=ptr3_1,_rc)
               call ESMF_FieldGet(field2,farrayPtr=ptr3_2,_rc)
               lb=lbound(ptr3_1)
               ub=ubound(ptr3_1)
               do i=1,size(ptr3_1,1)
                  do j=1,size(ptr3_1,2)
                     do k=lb(3),ub(3)
                        if (abs(ptr3_1(i,j,k)-ptr3_2(i,j,k)) .gt. tol) then
                           foundDiff(ii)=.true.
                           exit
                        end if
                     enddo
                  enddo
               enddo
            end if
            if (foundDiff(ii)) then
               _fail('found difference when compare state')
            end if
         enddo

         _return(ESMF_SUCCESS)

      end subroutine compare_bundle

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

    function create_cf(grid_name,im_world,jm_world,nx,ny,lm,cs_stretch_param,rc) result(cf)
       use MAPL_ConfigMod
       type(ESMF_Config)              :: cf
       character(len=*), intent(in) :: grid_name
       integer, intent(in)          :: im_world,jm_world
       integer, intent(in)          :: nx,ny
       integer, intent(in)          :: lm
       real, intent(in)             :: cs_stretch_param(3)
       integer, optional, intent(out) :: rc

       integer :: status
       character(len=2) :: pole,dateline
       integer :: nn

       nn = len_trim(grid_name)
       dateline=grid_name(nn-1:nn)
       pole=grid_name(1:2)

       cf = MAPL_ConfigCreate(_rc)
       call MAPL_ConfigSetAttribute(cf,value=NX, label=trim(grid_name)//".other:",_rc)
       call MAPL_ConfigSetAttribute(cf,value=NX, label=trim(grid_name)//".NX:",_rc)
       call MAPL_ConfigSetAttribute(cf,value=lm, label=trim(grid_name)//".LM:",_rc)
       if (jm_world==6*im_world) then
          call MAPL_ConfigSetAttribute(cf,value="Cubed-Sphere", label=trim(grid_name)//".GRID_TYPE:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=6, label=trim(grid_name)//".NF:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=ny/6, label=trim(grid_name)//".NY:",_rc)
          if (any(cs_stretch_param/=cs_stretch_uninit)) then
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(1),label=trim(grid_name)//".STRETCH_FACTOR:",_rc)
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(2),label=trim(grid_name)//".TARGET_LON:",_rc)
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(3),label=trim(grid_name)//".TARGET_LAT:",_rc)
          end if

       else
          call MAPL_ConfigSetAttribute(cf,value="LatLon", label=trim(grid_name)//".GRID_TYPE:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=jm_world,label=trim(grid_name)//".JM_WORLD:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=ny, label=trim(grid_name)//".NY:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=pole, label=trim(grid_name)//".POLE:",_rc)
          call MAPL_ConfigSetAttribute(cf,value=dateline, label=trim(grid_name)//".DATELINE:",_rc)
       end if

       _return(_success)
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

    end module BundleTestSupport

! This is how you can "reset" the MAPL_Generic.h verify bits for a program.
! Program must be at the end of the file to do this and everything else in a module

#define I_AM_MAIN
#include "MAPL_Generic.h"

    program ut_ReGridding

       use BundleTestSupport
       implicit none

!CONTAINS

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)     :: grid_new
   type(ESMF_VM)       :: vm             ! ESMF Virtual Machine

!  Basic information about the parallel environment
!         PET = Persistent Execution Threads
!  In the current implementation, a PET is equivalent
!  to an MPI process
!  ------------------------------------------------
   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status
   integer :: Nx,Ny,nargs
   integer :: IM_World_new, JM_World_new, lm_world

   type(ESMF_FieldBundle) :: bundle,bundle_new
   type(ESMF_Field) :: field
   type(ESMF_Time) :: time
   type(ESMF_TimeInterval) :: timeInterval
   type(ESMF_Clock) :: clock

   character(len=ESMF_MAXSTR) :: filename

   integer :: i

   character(len=2) :: pole_new,dateline_new
   character(len=ESMF_MAXSTR) :: gridname
   character(len=ESMF_MAXPATHLEN) :: str,astr
   type(ESMF_CONFIG) :: cfoutput

   type(FieldBundleWriter) :: newWriter
   type(ServerManager) :: io_server
   real, pointer :: ptr2d(:,:),ptr3d(:,:,:)
   real :: cs_stretch_param(3)

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF's automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_NONE, vm=vm, _rc)
    call ESMF_VMGet(vm, localPET=myPET, petCount=nPet, _rc)
    call MAPL_Initialize(_rc)

    call io_server%initialize(mpi_comm_world)

    nx=1
    ny=6
    cs_stretch_param=cs_stretch_uninit
    nargs = command_argument_count()
    do i=1,nargs
      call get_command_argument(i,str)
      select case(trim(str))
      case('-ogrid')
         call get_command_argument(i+1,Gridname)
      case('-nx')
         call get_command_argument(i+1,astr)
         read(astr,*)nx
      case('-ny')
         call get_command_argument(i+1,astr)
         read(astr,*)ny
      case('-o')
         call get_command_argument(i+1,filename)
      end select
    enddo

    call MAPL_GetNodeInfo (comm=MPI_COMM_WORLD, _rc)

    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, _rc )

    call ESMF_TimeSet(time, yy=2000, mm=3, dd=15,  h=21,  m=0, s=0,_rc)
    call ESMF_TimeIntervalSet( TimeInterval, h=6, m=0, s=0, _rc )
    Clock = ESMF_ClockCreate ( name="Eric", timeStep=TimeInterval, &
                               startTime=time, _rc )

    call UnpackGridName(Gridname,im_world_new,jm_world_new,dateline_new,pole_new)

    lm_world=3
    cfoutput = create_cf(trim(gridname),im_world_new,jm_world_new,nx,ny,lm_world,cs_stretch_param,_rc)
    grid_new=grid_manager%make_grid(cfoutput,prefix=trim(gridname)//".",_rc)
    bundle=ESMF_FieldBundleCreate(name="cfio_bundle",_rc)
    call ESMF_FieldBundleSet(bundle,grid=grid_new,_rc)
    bundle_new=ESMF_FieldBundleCreate(name="cfio_bundle",_rc)
    call ESMF_FieldBundleSet(bundle_new,grid=grid_new,_rc)

    field=ESMF_FieldCreate(grid=grid_new,typekind=ESMF_TYPEKIND_R4,name="f2d",_rc)
    call ESMF_AttributeSet(FIELD,'LONG_NAME','what_am_i',_rc)
    call ESMF_AttributeSet(FIELD,'UNITS','NA',_rc)
    call ESMF_AttributeSet(FIELD,'DIMS',MAPL_DimsHorzOnly,_rc)
    call ESMF_AttributeSet(FIELD,'VLOCATION',MAPL_VLocationNone,_rc)
    call ESMF_FieldGet(field,farrayPtr=ptr2d,_rc)
    ptr2d=17.0
    call MAPL_FieldBundleAdd(bundle,field,_rc)

    field=ESMF_FieldCreate(grid=grid_new,typekind=ESMF_TYPEKIND_R4,name="f3d", &
      ungriddedLBound=[1],ungriddedUBound=[lm_world],_rc)
    call ESMF_AttributeSet(FIELD,'LONG_NAME','what_am_i',_rc)
    call ESMF_AttributeSet(FIELD,'UNITS','NA',_rc)
    call ESMF_AttributeSet(FIELD,'DIMS',MAPL_DimsHorzVert,_rc)
    call ESMF_AttributeSet(FIELD,'VLOCATION',MAPL_VLocationCenter,_rc)
    call ESMF_FieldGet(field,farrayPtr=ptr3d,_rc)
    ptr3d=17.0
    call MAPL_FieldBundleAdd(bundle,field,_rc)


    call newWriter%create_from_bundle(bundle,clock,filename,_rc)
    call newWriter%write_to_file(_rc)
    call MAPL_Read_bundle(bundle_new,trim(filename),time=time,_rc)

    call Compare_Bundle(bundle,bundle_new,1.0e6,_rc)

    call io_server%finalize()
    call MAPL_Finalize(_rc)
    call ESMF_Finalize(_rc)

    end program ut_ReGridding

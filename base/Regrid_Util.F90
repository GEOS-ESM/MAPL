#include "MAPL_Generic.h"

   Program ut_ReGridding

   use ESMF
   use MPI
   use MAPL_ExceptionHandling
 
   implicit NONE

   type(ESMF_RouteHandle) :: rh,rh1
   type(ESMF_Array)    :: arr1,arr2,arr3,arr4,array
   type(ESMF_Array)    :: brr1,brr2

   call main()
    
CONTAINS
 
    subroutine main()

!CONTAINS

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_VM)       :: vm             ! ESMF Virtual Machine
   type(ESMF_DistGrid) :: dg1,dg2,dg3
   type(ESMF_DeLayout) :: de1,de2,de3
   logical :: g1,g2,g3,g12,g13,g123
   type(ESMF_State) :: state
   type(ESMF_FIeld) :: field,field2

   type(ESMF_Grid) :: grid1,grid2,gridall
   real(ESMF_KIND_R8), pointer :: coords(:,:)
   real, pointer :: c(:,:),c2(:,:,:)

!  Basic information about the parallel environment
!         PET = Persistent Execution Threads
!  In the current implementation, a PET is equivalent 
!  to an MPI process
!  ------------------------------------------------
   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc,im,jm
   character(len=100) :: last_name

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF's automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_MULTI, vm=vm, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet(vm, localPET=myPET, petCount=nPet)

    g1=.false.
    g2=.false.
    g3=.false.
    g12=.false.
    g13=.false.
    g123=.false.
    im=10
    jm=10

    if (mypet==0) g1=.true. 
    if (mypet==1) g2=.true. 
    if (mypet==2) g3=.true.
    if (mypet==0.or.mypet==1) g12=.true.
    if (mypet==0.or.mypet==2) g13=.true.
    if (mypet==0.or.mypet==1.or.mypet==2) g123=.true.
    de1=ESMF_DELayoutCreate(deCount=1,petList=[0],rc=status)
    _VERIFY(STATUS)
    dg1=ESMF_DistGridCreate([1,1],[im,jm],regDecomp=[1,1],delayout=de1,rc=status)
    _VERIFY(STATUS)

    state=ESMF_StateCreate()
    if (g1) then
       Grid1=ESMF_GridCreate(dg1,rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeSet(grid1,name='GRID_LM',value=72,rc=status)
       _VERIFY(STATUS)
       call ESMF_GridAddCoord(grid1,rc=status)
       _VERIFY(STATUS)
       call ESMF_GridGetCoord(grid1,coordDim=1,localDE=0, &
               farrayPtr=coords,rc=status)
       _VERIFY(STATUS)
       coords=17.0d0
       call ESMF_GridGetCoord(grid1,coordDim=2,localDE=0, &
               farrayPtr=coords,rc=status)
       _VERIFY(STATUS)
       coords=17.0d0

       field=ESMF_FieldCreate(grid1,typekind=ESMF_TYPEKIND_R4,name="BBB",rc=status)
       _VERIFY(STATUS)
       call ESMF_StateAdd(state,[field],rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field,farrayptr=c,rc=status)
       _VERIFY(STATUS)
       c=2.0
       
    end if
    call ESMF_StateReconcile(state,rc=status)
    _VERIFY(STATUS)
    call ESMF_StateGet(state,"BBB",field,rc=status)
    _VERIFY(STATUS)
    call ESMF_FieldGet(field,grid=grid1,rc=status)
    _VERIFY(STATUS)
    call ESMF_GridGet(grid1,distGrid=dg1,rc=status)
    _VERIFY(STATUS)

    dg2 = ESMF_DistGridCreate(dg1,vm=vm,rc=status)
    _VERIFY(STATUS)
    grid2=ESMF_GridCreate(grid1,dg2,name="boblala",copyAttributes=.true.,rc=status)
    _VERIFY(STATUS)
    field2=ESMF_FieldCreate(grid2,typekind=ESMF_TYPEKIND_R4,name="CCC",rc=status)
    _VERIFY(STATUS)
    if (g2) then
       call ESMF_FieldGet(field2,farrayptr=c,rc=status)
       _VERIFY(STATUS)
       write(*,*)"bmaa ",size(c,1),size(c,2)
    end if
     
    call ESMF_Finalize()
    end subroutine main

    subroutine server()
       real, pointer :: a(:,:),b(:,:)
       write(*,*)"server1"
       call ESMF_ArraySMM(dstArray=arr2,routeHandle=rh,termorderflag=ESMF_TERMORDER_SRCSEQ)
       call ESMF_ArraySMM(dstArray=brr2,routeHandle=rh,termorderflag=ESMF_TERMORDER_SRCSEQ)
       write(*,*)"server2"
       !call sleep(10)
       write(*,*)"server3"
       call ESMF_ArrayGet(arr2,farrayPtr=a)
       call ESMF_ArrayGet(brr2,farrayPtr=b)
       write(*,*)"f: ",maxval(a),maxval(b)

    end subroutine server


    subroutine client(x)
       real :: x
       real, pointer :: a(:,:),b(:,:)
       write(*,*)"client1"
       call ESMF_ArrayGet(arr1,farrayPtr=a)
       call ESMF_ArrayGet(brr1,farrayPtr=b)
       a=x
       b=x
       !call sleep(5)
       write(*,*)"client2"
       call ESMF_ArraySMM(srcArray=arr1,routeHandle=rh,termorderflag=ESMF_TERMORDER_SRCSEQ)
       call ESMF_ArraySMM(srcArray=brr1,routeHandle=rh,termorderflag=ESMF_TERMORDER_SRCSEQ)


    end subroutine client

 
    end program ut_ReGridding 


!  $Id$

#include "MAPL_ErrLog.h"

!BOP

! !MODULE: MAPL_Comms -- A Module to parallel comunications until ESMF fully supports it


! !INTERFACE:

module MAPL_CommsMod

  use ESMF
  use MAPL_BaseMod
  use MAPL_ShmemMod
  use MAPL_ConstantsMod
  use MAPL_ErrorHandlingMod
  implicit none
  private

  public MAPL_Abort
  public MAPL_CommsBcast
  public MAPL_CommsScatterV
  public MAPL_CommsGatherV
  public MAPL_CommsAllGather
  public MAPL_CommsAllGatherV
  public MAPL_CommsAllReduceMin
  public MAPL_CommsAllReduceMax
  public MAPL_CommsAllReduceSum
  public MAPL_CommsSend
  public MAPL_CommsRecv
  public MAPL_CommsSendRecv
  public MAPL_AM_I_ROOT
  public MAPL_NPES
  public ArrayGather
  public ArrayScatter
  public MAPL_root

  public MAPL_CreateRequest
  public MAPL_CommRequest
  public MAPL_ArrayIGather
  public MAPL_ArrayIScatter
  public MAPL_CollectiveWait
  public MAPL_CollectiveScatter3D
  public MAPL_CollectiveGather3D
  public MAPL_RoundRobinPEList
  public MAPL_BcastShared

  integer, parameter, public :: MAPL_Unknown   = 0
  integer, parameter, public :: MAPL_IsGather  = 1
  integer, parameter, public :: MAPL_IsScatter = 2

  type ArrPtr
     real, pointer :: A(:,:)
  end type ArrPtr

  public ArrPtr

  type MAPL_CommRequest
     integer, pointer :: i1(:),in(:),j1(:),jn(:),im(:),jm(:)
     integer          :: im_world, jm_world, im0, jm0
     integer, pointer :: recv(:)=>null()
     integer, pointer :: send(:)=>null()
     real, pointer    :: var(:)=>null()
     real, pointer    :: DstArray(:,:)=>null()
     real, pointer    :: Local_Array(:,:)=>null()
     real, pointer    :: Trans_Array(:,:,:)=>null()
     real, pointer    :: Read_Array(:,:)=>null()
     type(ArrPtr), pointer :: Buff(:)
     integer          :: nDEs, MYPE, comm, root
     logical          :: active=.false., amRoot=.false.
     logical          :: IsPrePosted
     integer          :: RequestType=MAPL_Unknown
     integer	      :: tag, s_rqst
  end type MAPL_CommRequest

  interface MAPL_Am_I_Root
     module procedure MAPL_Am_I_Root_Layout
     module procedure MAPL_Am_I_Root_Vm
  end interface

  interface MAPL_NPES
     module procedure MAPL_NPES_Layout
     module procedure MAPL_NPES_Vm
  end interface

  interface MAPL_CommsBcast
     module procedure MAPL_CommsBcast_STRING_0
     module procedure MAPL_CommsBcast_L4_0
     module procedure MAPL_CommsBcast_I4_0
     module procedure MAPL_CommsBcast_R4_0
     module procedure MAPL_CommsBcast_R8_0
     module procedure MAPL_CommsBcast_I4_1
     module procedure MAPL_CommsBcast_R4_1
     module procedure MAPL_CommsBcast_R8_1
     module procedure MAPL_CommsBcast_I4_2
     module procedure MAPL_CommsBcast_R4_2
     module procedure MAPL_CommsBcast_R8_2
     module procedure MAPL_CommsBcastVm_STRING_0
     module procedure MAPL_CommsBcastVm_L4_0
     module procedure MAPL_CommsBcastVm_I4_0
     module procedure MAPL_CommsBcastVm_R4_0
     module procedure MAPL_CommsBcastVm_R8_0
     module procedure MAPL_CommsBcastVm_I4_1
     module procedure MAPL_CommsBcastVm_R4_1
     module procedure MAPL_CommsBcastVm_R8_1
     module procedure MAPL_CommsBcastVm_I4_2
     module procedure MAPL_CommsBcastVm_R4_2
     module procedure MAPL_CommsBcastVm_R8_2
  end interface

  interface MAPL_BcastShared
     module procedure MAPL_BcastShared_1DR4
     module procedure MAPL_BcastShared_2DR4
  end interface

  interface MAPL_CommsScatterV
     module procedure MAPL_CommsScatterV_I4_1
     module procedure MAPL_CommsScatterV_R4_1
     module procedure MAPL_CommsScatterV_R4_2
     module procedure MAPL_CommsScatterV_R8_1
     module procedure MAPL_CommsScatterV_R8_2
  end interface

  interface MAPL_CommsGatherV
     module procedure MAPL_CommsGatherV_I4_1
     module procedure MAPL_CommsGatherV_R4_1
     module procedure MAPL_CommsGatherV_R4_2
     module procedure MAPL_CommsGatherV_R8_1
     module procedure MAPL_CommsGatherV_R8_2
  end interface

  interface MAPL_CommsAllGather
     module procedure MAPL_CommsAllGather_I4_1
     module procedure MAPL_CommsAllGather_L4_1
  end interface

  interface MAPL_ArrayIGather
     module procedure MAPL_ArrayIGather_R4_2
  end interface

  interface MAPL_ArrayIScatter
     module procedure MAPL_ArrayIScatter_R4_2
  end interface
  
  interface MAPL_CommsAllGatherV
     module procedure MAPL_CommsAllGatherV_I4_1
     module procedure MAPL_CommsAllGatherV_R4_1
     module procedure MAPL_CommsAllGatherV_R8_1
  end interface

  interface MAPL_CommsAllReduceMin
     module procedure MAPL_CommsAllReduceMin_I4_0
     module procedure MAPL_CommsAllReduceMin_R4_0
     module procedure MAPL_CommsAllReduceMin_R8_0
     module procedure MAPL_CommsAllReduceMin_I4_1
     module procedure MAPL_CommsAllReduceMin_R4_1
     module procedure MAPL_CommsAllReduceMin_R8_1
     module procedure MAPL_CommsAllReduceMin_I4_2
     module procedure MAPL_CommsAllReduceMin_R4_2
     module procedure MAPL_CommsAllReduceMin_R8_2
  end interface

  interface MAPL_CommsAllReduceMax
     module procedure MAPL_CommsAllReduceMax_I4_0
     module procedure MAPL_CommsAllReduceMax_R4_0
     module procedure MAPL_CommsAllReduceMax_R8_0
     module procedure MAPL_CommsAllReduceMax_I4_1
     module procedure MAPL_CommsAllReduceMax_R4_1
     module procedure MAPL_CommsAllReduceMax_R8_1
     module procedure MAPL_CommsAllReduceMax_I4_2
     module procedure MAPL_CommsAllReduceMax_R4_2
     module procedure MAPL_CommsAllReduceMax_R8_2
  end interface

  interface MAPL_CommsAllReduceSum
     module procedure MAPL_CommsAllReduceSum_I4_0
     module procedure MAPL_CommsAllReduceSum_R4_0
     module procedure MAPL_CommsAllReduceSum_R8_0
     module procedure MAPL_CommsAllReduceSum_I4_1
     module procedure MAPL_CommsAllReduceSum_R4_1
     module procedure MAPL_CommsAllReduceSum_R8_1
     module procedure MAPL_CommsAllReduceSum_I4_2
     module procedure MAPL_CommsAllReduceSum_R4_2
     module procedure MAPL_CommsAllReduceSum_R8_2
  end interface

  interface MAPL_CommsSend
     module procedure MAPL_CommsSend_I4_0
     module procedure MAPL_CommsSend_I4_1
     module procedure MAPL_CommsSend_R4_1
     module procedure MAPL_CommsSend_R4_2
     module procedure MAPL_CommsSend_R8_1
     module procedure MAPL_CommsSend_R8_2
  end interface

  interface MAPL_CommsRecv
     module procedure MAPL_CommsRecv_I4_0
     module procedure MAPL_CommsRecv_I4_1
     module procedure MAPL_CommsRecv_R4_1
     module procedure MAPL_CommsRecv_R4_2
     module procedure MAPL_CommsRecv_R8_1
     module procedure MAPL_CommsRecv_R8_2
  end interface

  interface MAPL_CommsSendRecv
     module procedure MAPL_CommsSendRecv_I4_0
     module procedure MAPL_CommsSendRecv_R4_1
     module procedure MAPL_CommsSendRecv_R4_2
     module procedure MAPL_CommsSendRecv_R8_1
     module procedure MAPL_CommsSendRecv_R8_2
  end interface

  interface ArrayScatter
     module procedure ArrayScatter_R4_1
     module procedure ArrayScatter_R8_1
     module procedure ArrayScatter_R4_2
     module procedure ArrayScatter_R8_2
     module procedure ArrayScatterRcvCnt_I4_1
     module procedure ArrayScatterRcvCnt_R4_1
  end interface

  interface ArrayGather
     module procedure ArrayGather_I4_1
     module procedure ArrayGather_R4_1
     module procedure ArrayGather_R8_1
     module procedure ArrayGather_R4_2
     module procedure ArrayGather_R8_2
     module procedure ArrayGatherRcvCnt_I4_1
     module procedure ArrayGatherRcvCnt_R4_1
  end interface

  include "mpif.h"

  integer, parameter :: MAPL_root=0
  integer, parameter :: msg_tag=11

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!---------------------------
!---------------------------
!---------------------------
  function MAPL_Am_I_Root_Vm(VM) result(R)
    type (ESMF_VM), optional :: VM
    logical                  :: R

    integer        :: deId
    integer        :: status
    type (ESMF_VM) :: VM_

    R = .false.
    if (present(VM)) then
       VM_ = VM
    else
       call ESMF_VMGetCurrent(vm_, rc=status)
    end if

    call ESMF_VMGet(vm_, localPet=deId, rc=status)
    if (deId == MAPL_Root) R = .true.

  end function MAPL_Am_I_Root_Vm

  function MAPL_Am_I_Root_Layout(layout) result(R)
    type (ESMF_DELayout) :: layout
    logical              :: R

    integer       :: status
    type (ESMF_VM) :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    R = MAPL_Am_I_Root(vm)

  end function MAPL_Am_I_Root_Layout


  subroutine MAPL_CreateRequest(grid, Root, request, tag, RequestType, &
                                DstArray, PrePost, hw, rc)
    type (ESMF_Grid),        intent(IN   ) :: grid
    integer,                 intent(IN   ) :: Root
    type (MAPL_CommRequest), intent(INOUT) :: request
    integer,                 intent(IN   ) :: tag, RequestType
    real, target, optional,  intent(IN   ) :: DstArray(:,:)
    logical,      optional,  intent(IN   ) :: PrePost
    integer,      optional,  intent(IN   ) :: hw
    integer,      optional,  intent(  OUT) :: rc
    
! Local variables

    integer                    :: status
    character(len=ESMF_MAXSTR) :: IAm='MAPL_CreateRequest'

    type (ESMF_VM)             :: VM
    type (ESMF_DistGrid)       :: distGrid

    integer, allocatable       :: AL(:,:), AU(:,:)
    integer                    :: count
    integer                    :: displs
    integer                    :: n
    integer                    :: myPE, nDEs
    integer                    :: gridRank
    integer                    :: comm
    integer                    :: hw_

! Begin
!------

    if (present(hw)) then
       hw_ = hw
    else
       hw_ = 0
    end if

    _ASSERT(.not.request%active, 'request is already active')

! Communicator info all comes from the ESMF VM
!---------------------------------------------

    call ESMF_VMGetCurrent(vm,                                RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet       (VM,       mpiCommunicator =comm,   RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet       (VM, localpet=MYPE, petcount=nDEs,  RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
    _VERIFY(STATUS)

! Does not support 1D grids
!--------------------------

    _ASSERT(gridRank > 1, 'rank 1 is not supported')


! Get the local grid bounds for all pes. We will use only
!   the first 2 dimensions.
!--------------------------------------------------------

    call ESMF_GridGet(GRID, distGrid=distGrid, RC=STATUS); _VERIFY(STATUS)

    allocate (AL(gridRank,0:nDEs-1), stat=STATUS)
    _VERIFY(STATUS)
    allocate (AU(gridRank,0:nDEs-1), stat=STATUS)
    _VERIFY(STATUS)

    call MAPL_DistGridGet (distgrid, minIndex=AL, maxIndex=AU, RC=STATUS); _VERIFY(STATUS)

! Allocate space for request variables
!-------------------------------------

    allocate (request%i1(0:nDEs-1),  stat=STATUS)
    _VERIFY(STATUS)
    allocate (request%in(0:nDEs-1),  stat=STATUS)
    _VERIFY(STATUS)
    allocate (request%j1(0:nDEs-1),  stat=STATUS)
    _VERIFY(STATUS)
    allocate (request%jn(0:nDEs-1),  stat=STATUS)
    _VERIFY(STATUS)
    allocate (request%im(0:nDEs-1),  stat=STATUS)
    _VERIFY(STATUS)
    allocate (request%jm(0:nDEs-1),  stat=STATUS)
    _VERIFY(STATUS)
    allocate (request%RECV (0:nDEs-1         ),        stat=STATUS)
    _VERIFY(STATUS)
    allocate (request%SEND (0:nDEs-1         ),        stat=STATUS)
    _VERIFY(STATUS)

! Fill the request variables
!---------------------------

    request%amRoot        =  (myPE == Root)
    request%active        = .true.
    request%nDEs          =  nDEs
    request%myPE          =  myPE
    request%comm          =  comm
    request%root          =  root
    request%RequestType   =  RequestType
    request%tag           =  tag
   
    request%I1 = AL(1,:)-hw_
    request%In = AU(1,:)+hw_
    request%J1 = AL(2,:)-hw_
    request%Jn = AU(2,:)+hw_
    request%IM = request%IN-request%I1+1
    request%JM = request%JN-request%J1+1

    request%IM_WORLD = request%IN(nDEs-1)- request%I1(0) + 1 - (2*hw_)
    request%JM_WORLD = request%JN(nDEs-1)- request%J1(0) + 1 - (2*hw_)
    request%IM0      = request%IN(mype  )- request%I1(mype) + 1
    request%JM0      = request%JN(mype  )- request%J1(mype) + 1

    if(present(PrePost)) then
       request%IsPrePosted = PrePost
    else
       request%IsPrePosted = .false.
    end if

    deallocate(AL,AU)

! Verify that we have a valid destination area
!---------------------------------------------

    if(requestType==MAPL_IsGather) then
       if(request%amRoot) then
          if(present(DstArray)) then
             request%DstArray => DstArray
             _ASSERT(all(shape(DstArray)==(/ request%IM_WORLD, request%JM_WORLD/)), 'inconsistent shape')
          else
             allocate(request%DstArray(request%IM_WORLD, request%JM_WORLD),stat=STATUS)
             _VERIFY(STATUS)
          end if
       endif
    elseif(requestType==MAPL_IsScatter) then
       if(present(DstArray)) then
          request%DstArray => DstArray
          _ASSERT(all(shape(DstArray)==(/ request%IM0     , request%JM0     /)), 'inconsistent shape')
       else
          allocate(request%DstArray(request%IM0 , request%JM0 ),stat=STATUS)
          _VERIFY(STATUS)
       end if
    else
       _ASSERT(.false., 'unsupported action')
    end if

! Allocate a contiguous buffer for communication
!-----------------------------------------------

    if(requestType==MAPL_IsGather .and. request%amRoot) then
       allocate (request%Var(0:request%IM_WORLD*request%JM_WORLD-1),      stat=STATUS)
       _VERIFY(STATUS)
    elseif(requestType==MAPL_IsScatter) then
       allocate (request%Var(0:request%IM0*request%JM0-1),      stat=STATUS)
       _VERIFY(STATUS)
    else
       allocate (request%Var(1),      stat=STATUS)
       _VERIFY(STATUS)
    endif

! We also PrePost the request here
!---------------------------------
    
    POST_REQUEST: if(request%IsPrePosted) then
       if(requestType==MAPL_IsGather) then
          if(request%amRoot) then
             displs = 0
             do n=0,nDEs-1
                count = request%IM(n)*request%JM(n)
                if(n /= mype) then
                   call MPI_IRecv(request%VAR(displs), count, MPI_REAL, &
                        n, tag, comm, request%recv(n), status)
                   _VERIFY(STATUS)
                end if
                displs = displs + count
             end do
          endif

       else
          if(.not.request%amRoot) then
             call MPI_IRecv(request%Var, size(request%Var), MPI_REAL, &
                  request%Root, tag, comm, request%recv(0), status)
             _VERIFY(STATUS)
          end if
       end if
    end if POST_REQUEST

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CreateRequest

!===================================================================

  subroutine MAPL_ArrayIGather_R4_2(local_array, request, rc)
    real,                    intent(IN   ) :: local_array (:,:)
    type (MAPL_CommRequest), intent(INOUT) :: request
    integer, optional,       intent(  OUT) :: rc
    
! Local variables

    integer                    :: status
    character(len=ESMF_MAXSTR) :: IAm='MAPL_ArrayIGather2d'

    integer  :: i1, in, j1, jn

    allocate(request%local_array(size(LOCAL_ARRAY,1),size(LOCAL_ARRAY,2)), stat=STATUS)
    _VERIFY(STATUS)

! In senders, copy input to contiguous buffer for safety
!-------------------------------------------------------

    request%local_array = local_array

    if(request%amRoot) then
       i1 = request%i1(request%mype)
       in = request%in(request%mype)
       j1 = request%j1(request%mype)
       jn = request%jn(request%mype)
       request%DstArray(i1:in,j1:jn) = local_array
    else
       call MPI_ISend(request%Local_Array, size(Local_Array), MPI_REAL, &
            request%root, request%tag, request%comm, request%send(0), status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ArrayIGather_R4_2

!===================================================================

  subroutine MAPL_ArrayIScatter_R4_2(global_array, request, hw, rc)
    real,                    intent(IN   ) :: global_array (:,:)
    type (MAPL_CommRequest), intent(INOUT) :: request
    integer, optional,       intent(   IN) :: hw
    integer, optional,       intent(  OUT) :: rc
    
! Local variables

    integer                    :: status
    character(len=ESMF_MAXSTR) :: IAm='MAPL_ArrayIScatter2d'


    integer                    :: i1,in,j1,jn
    integer                    :: n, count, hw_, j
    real, allocatable          :: global_array_(:,:)

    if (present(hw)) then
       hw_ = hw
    else
       hw_ = 0
    end if

! Post sends from all processors except root
!-------------------------------------------

    if(request%amRoot) then
       !if have halo, make local copy and halo global
       if (hw_ > 0) then
          allocate(Global_Array_(1-hw_:request%im_world+hw_,1-hw_:request%jm_world+hw_))
          Global_Array_(1:request%im_world,1:request%jm_world) = Global_Array
          do j=1,hw_
             ! x-direction
             Global_Array_(1-j,:) = Global_Array_(request%im_world-j+1,:)
             Global_Array_(request%im_world+j,:) = Global_Array_(j,:)
             ! y-direction
             Global_Array_(:,1-j) = MAPL_UNDEF
             Global_Array_(:,request%jm_world+j) = MAPL_UNDEF
          enddo
       endif
       allocate(request%Buff(0:request%nDEs-1))
       PEs: do n=0,request%nDEs-1
          count = request%IM(n)*request%JM(n)
          i1 = request%i1(n)
          in = request%in(n)
          j1 = request%j1(n)
          jn = request%jn(n)
          if(n == request%mype) then
             if (hw_ > 0) then
                 request%DstArray = Global_Array_(i1:in,j1:jn)
             else
                 request%DstArray = Global_Array(i1:in,j1:jn)
             end if
          else
             allocate(request%Buff(n)%A(request%im(n), request%jm(n))) 
             if (hw_ > 0) then
                  request%Buff(n)%A = Global_Array_(i1:in,j1:jn)
             else
                  request%Buff(n)%A = Global_Array(i1:in,j1:jn)
             end if
             call MPI_ISend(request%Buff(n)%A, count, MPI_REAL, &
                  n, request%tag, request%comm, request%send(n),  status)
             _VERIFY(STATUS)
          end if
       end do PEs
       if (hw_ > 0) deallocate(Global_Array_)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ArrayIScatter_R4_2

!=========================================================

  subroutine MAPL_CollectiveWait(request, DstArray, rc)
    type (MAPL_COMMRequest), intent(INOUT) :: request
    real, pointer, optional                :: DstArray(:,:)
    integer, optional,       intent(  OUT) :: rc

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='MAPL_CollectiveWait'

    integer               :: i,j,k,n
    integer               :: count
 
    REQUEST_TYPE: if(request%RequestType==MAPL_IsGather) then

       ROOT_GATH: if(request%amRoot) then
          k = 0
          PE_GATH: do n=0,request%nDEs-1
             count = request%IM(n)*request%JM(n)
             if(request%mype/=n) then
                if(request%IsPrePosted) then
                   call MPI_Wait(request%recv(n),MPI_STATUS_IGNORE,status)
                   _VERIFY(STATUS)
                else
                   call MPI_Recv(request%var(k), count, MPI_REAL, &
                        n, request%tag, request%comm, MPI_STATUS_IGNORE, status)
                   _VERIFY(STATUS)
                endif
                do J=request%J1(n),request%JN(n)
                   do I=request%I1(n),request%IN(n)
                      request%DstArray(I,J) = request%var(k)
                      k = k+1
                   end do
                end do
             else
                k = k + count
             end if
          end do PE_GATH
          if(present(DstArray)) DstArray => request%DstArray
       else
          call MPI_WAIT(request%send(0),MPI_STATUS_IGNORE,status)
          _VERIFY(STATUS)
       endif ROOT_GATH

    elseif(request%RequestType==MAPL_IsScatter) then

       ROOT_SCAT: if(.not.request%amRoot) then
          if(request%IsPrePosted) then
             call MPI_Wait(request%recv(0),MPI_STATUS_IGNORE,status)
             _VERIFY(STATUS)
          else
             call MPI_Recv(request%Var, size(request%Var), MPI_REAL, &
                           request%Root, request%tag, request%comm,  &
                           MPI_STATUS_IGNORE, status)
          endif
          k=0
          do J=1,request%JM0
             do I=1,request%IM0
                request%DstArray(I,J) = request%var(k)
                k = k+1
             end do
          end do

       else
             PE_SCAT: do n=0,request%nDEs-1
                if(n /= request%mype) then
                   call MPI_Wait(request%send(n),MPI_STATUS_IGNORE,status)
                   _VERIFY(STATUS)
                   deallocate(request%buff(n)%A)
                end if
             end do PE_SCAT
          deallocate(request%Buff)
       end if ROOT_SCAT

       if(present(DstArray)) DstArray => request%DstArray
    end if REQUEST_TYPE

! Destroy the request
!--------------------

    deallocate(request%var )
    deallocate(request%recv)
    deallocate(request%send)
    deallocate(request%i1  )
    deallocate(request%in  )
    deallocate(request%j1  )
    deallocate(request%jn  )
    deallocate(request%im  )
    deallocate(request%jm  )

    nullify(request%var     )
    nullify(request%send    )
    nullify(request%recv    )
    nullify(request%DstArray)

    if(associated(request%Local_Array)) deallocate(request%Local_Array)
    nullify(request%Local_Array)

    request%active = .false.

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CollectiveWait

!---------------------------
!---------------------------
!---------------------------


  subroutine MAPL_CollectiveGather3D(Grid, LocArray, GlobArray, &
                                     CoresPerNode, rc)

    type (ESMF_Grid),        intent(INout) :: Grid
    real,                    intent(IN   ) :: LocArray(:,:,:)
    real, pointer                          :: GlobArray(:,:,:)
    integer, optional,       intent(In   ) :: CoresPerNode
    integer, optional,       intent(  OUT) :: rc

! Locals
!-------

    integer                       :: status
    character(len=ESMF_MAXSTR)    :: IAm='MAPL_CollectiveGather3D'

    type (MAPL_CommRequest)       :: reqs(size(LocArray,3))
    integer                       :: root(size(LocArray,3))
    integer                       :: NumCores, Nnodes
    integer                       :: nn
    integer                       :: LM, L, nc, npes, mype, dims(5)
    type(ESMF_VM)                 :: VM
    integer                       :: comm
    
! Begin
!------

    _ASSERT(.not.associated(GlobArray), 'GlobalArray already associated')

    call ESMF_VMGetCurrent(VM,         RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet(VM, petcount=npes, localpet=MYPE, mpiCommunicator=comm, RC=STATUS)
    _VERIFY(STATUS)

    if(present(CoresPerNode)) then
       NumCores = CoresPerNode
    else
       NumCores = MAPL_CoresPerNodeGet(comm,rc=status)
       _VERIFY(STATUS)
    end if

    LM     = size(LocArray,3)

    nNodes = size(MAPL_NodeRankList)
    call MAPL_RoundRobinPEList(Root, nNodes, RC=STATUS)
    _VERIFY(STATUS)

    if(any(root==mype)) then
       call MAPL_GridGet ( grid, globalCellCountPerDim=DIMS, RC=STATUS)
       _VERIFY(STATUS)
       nc = count(Root==mype)
       allocate(GlobArray(dims(1),dims(2),nc),stat=STATUS)
       _VERIFY(STATUS)
    else
       allocate(GlobArray(1,1,1)             ,stat=STATUS)
       _VERIFY(STATUS)
    endif

    nn = 0

    do L=1,LM
       if(root(L) == mype) then
          nn = nn + 1
          call MAPL_CreateRequest(GRID, Root(L), reqs(L), tag=L,    &
                               RequestType=MAPL_IsGather,           &
                               DstArray=GlobArray(:,:,nn),          &
                               PrePost=.true.,             RC=STATUS)
          _VERIFY(STATUS)
       else
          call MAPL_CreateRequest(GRID, Root(L), reqs(L), tag=L,    &
                               RequestType=MAPL_IsGather,           &
                               DstArray=GlobArray(:,:,1),           &
                               PrePost=.true.,             RC=STATUS)
          _VERIFY(STATUS)
       end if
    enddo  ! Do not fuse with next

    do L=1,LM
       call MAPL_ArrayIGather (LocArray(:,:,L), reqs(L),  RC=STATUS)
       _VERIFY(STATUS)
    enddo  ! Do not fuse with next

    do L=1,LM
       call MAPL_CollectiveWait(reqs(L), rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CollectiveGather3D


  subroutine MAPL_CollectiveScatter3D(Grid, GlobArray, LocArray, hw, rc)

    type (ESMF_Grid),        intent(IN   ) :: Grid
    real, target,            intent(INOUT) :: LocArray(:,:,:)
    real,                    intent(IN   ) :: GlobArray(:,:,:)
    integer, optional,       intent(IN   ) :: hw
    integer, optional,       intent(  OUT) :: rc

! Locals
!-------

    integer                       :: status
    character(len=ESMF_MAXSTR)    :: IAm='MAPL_CollectiveScatter3D'

    type (MAPL_CommRequest)       :: reqs(size(LocArray,3))
    integer                       :: root(size(LocArray,3))
    integer                       :: nNodes
    integer                       :: LM, L, nc, npes, mype
    integer                       :: nn
    type(ESMF_VM)                 :: VM
    logical                       :: HaveGlobal
    integer                       :: comm
    integer                       :: hw_
    
! Begin
!------

    call ESMF_VMGetCurrent(VM,         RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet(VM, petcount=npes, localpet=MYPE, mpiCommunicator=comm, RC=STATUS)
    _VERIFY(STATUS)

    if(present(hw)) then
       hw_ = hw
    else
       hw_ = 0
    endif

    nNodes = size(MAPL_NodeRankList)
    call MAPL_RoundRobinPEList(Root, nNodes, RC=STATUS)
    _VERIFY(STATUS)

    LM = size(LocArray,3)
    NC = count(Root==mype)

    HaveGlobal = NC>0

    do L=1,LM
       call MAPL_CreateRequest(GRID, Root(L), reqs(L), tag=L,       &
                               RequestType=MAPL_IsScatter,          &
                               DstArray=LocArray(:,:,L),            &
                               PrePost=.true., hw=hw_, RC=STATUS)
       _VERIFY(STATUS)
    enddo

    if(HaveGlobal) then
       _ASSERT(size(GlobArray,3)==NC, 'inconsisntent rank')
       
       nn = 0
       do L=1,LM
          if(Root(L)==mype) then

             nn = nn + 1
             call MAPL_ArrayIScatter (GlobArray(:,:,nn), reqs(L), hw=hw_, RC=STATUS)
             _VERIFY(STATUS)
             if(nn==NC) exit
          endif
       enddo
    end if

    do L=1,LM
       call MAPL_CollectiveWait(reqs(L), rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CollectiveScatter3D

  subroutine MAPL_RoundRobinPEList(List,nNodes,Root,UseFirstRank,FirstRank,RC)
    integer,           intent(  OUT) :: List(:)
    integer,           intent(IN   ) :: nNodes
    integer, optional, intent(IN   ) :: Root
    logical, optional, intent(IN   ) :: UseFirstRank
    integer, optional, intent(out  ) :: FirstRank
    integer, optional, intent(  OUT) :: RC

    integer                    :: status
    character(len=ESMF_MAXSTR) :: IAm='MAPL_RoundRobinPEList'
    integer, allocatable :: filled(:),nPerNode(:)
    integer :: i,n,nlist,locRoot
    logical :: gotFirstRank,lUseFirstRank

    if (present(Root)) then
       locRoot = Root
    else
       locRoot = 1
    endif
    if (present(UseFirstRank)) then
       lUseFirstRank=UseFirstRank
    else
       lUseFirstRank=.true.
    end if
    gotFirstRank = .false.
    if (present(UseFirstRank)) then
       lUseFirstRank=UseFirstRank
    else
       lUseFirstRank=.true.
    end if

    allocate(filled(nNodes),nPerNode(nNodes),stat=status)
    _VERIFY(STATUS)
    do i=1,nNodes
       nPerNode(i) = size(MAPL_NodeRankList(locRoot+i-1)%rank)
       if (lUseFirstRank) then
          filled(i)=0
       else
          filled(i)=MAPL_GetNewRank(locRoot+i-1,rc=status)-1
          _VERIFY(status)
       end if
    enddo
    nlist = size(list)
    n=0
    do
       do i=1,nNodes
          if (filled(i) < size(MAPL_NodeRankList(locRoot+i-1)%rank)) then
             filled(i) = filled(i) + 1
             n=n+1
             list(n) = MAPL_NodeRankList(locRoot+i-1)%rank(filled(i))
             if (.not.gotFirstRank .and. present(FirstRank)) then
                gotFirstRank=.true.
                FirstRank = list(n)
             end if
          end if

          if (n == nlist) exit
       enddo

       if (n == nlist) exit
       if (All(filled == nPerNode)) filled = 0
    enddo

    deallocate(filled,nPerNode)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_RoundRobinPEList

!---------------------------
!---------------------------
!---------------------------
  function MAPL_NPES_Vm(VM) result(R)
    type (ESMF_VM) :: VM
    integer        :: R

    integer       :: petCnt
    integer       :: status

    call ESMF_VMGet(vm, petCount=petCnt, rc=status)
    R = petCnt

    return
  end function MAPL_NPES_Vm

  function MAPL_NPES_Layout(layout) result(R)
    type (ESMF_DELayout), optional :: layout
    integer                        :: R

    integer       :: status
    type(ESMF_VM) :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    R = MAPL_NPES_Vm(vm)

    return
  end function MAPL_NPES_Layout



!--BCAST -----------------

  subroutine MAPL_CommsBcast_STRING_0( layout, data, N, ROOT, RC)
    type (ESMF_DELayout)                         :: layout
    character(len=*),   intent(INOUT)            :: data

    integer,            intent(in   )            :: N
    integer,            intent(in   )            :: ROOT
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_Bcast'
    integer                               :: status

    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    _VERIFY(STATUS)

    call MAPL_CommsBcast(vm, data=data, N=N, Root=Root, RC=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  END SUBROUTINE MAPL_CommsBcast_STRING_0

  subroutine MAPL_CommsBcastVM_STRING_0( vm, data, N, ROOT,RC)
    type (ESMF_VM)                               :: vm
    character(len=*),   intent(INOUT)            :: data

    integer,            intent(in   )            :: N
    integer,            intent(in   )            :: ROOT
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_BcastVM'
    character(len=N)                      :: tmpString
    integer                               :: slen
    integer                               :: status
    integer                               :: comm
    integer                               :: deId

    call ESMF_VMGet(vm, mpiCommunicator=COMM, localPet=deId, rc=status)
    _VERIFY(STATUS)

    tmpString = data
    if (deId == Root) then
       slen = len_trim(tmpString)
    end if

    call MPI_Bcast(slen, 1, MPI_INTEGER, ROOT, COMM, status)
    _VERIFY(STATUS)

    _ASSERT(slen <= N, 'exceeded string length')

    call MPI_Bcast(tmpString, slen, MPI_BYTE, ROOT, COMM, STATUS)
    _VERIFY(STATUS)

    data = ""
    data = tmpString(1:slen)

    _RETURN(ESMF_SUCCESS)

  END SUBROUTINE MAPL_CommsBcastVM_STRING_0

  subroutine MAPL_BcastShared_1DR4(VM, Data, N, Root, RootOnly, rc)
    type(ESMF_VM) :: VM
    real,    pointer,  intent(INOUT) :: Data(:)
    integer,           intent(IN   ) :: N
    integer, optional, intent(IN   ) :: Root
    logical,           intent(IN   ) :: RootOnly
    integer, optional, intent(  OUT) :: rc


    integer :: status
    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_BcastShared'


    if(.not.MAPL_ShmInitialized) then
       if (RootOnly) then
          _RETURN(ESMF_SUCCESS)
       end if
       call MAPL_CommsBcast(vm, DATA=Data, N=N, ROOT=Root, RC=status)
       _RETURN(STATUS)
    else
       call MAPL_SyncSharedMemory(RC=STATUS)
       _VERIFY(STATUS)
       call MAPL_BroadcastToNodes(Data, N=N, ROOT=Root, rc=status)
       _VERIFY(STATUS)
       call MAPL_SyncSharedMemory(RC=STATUS)
       _VERIFY(STATUS)
    endif

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_BcastShared_1DR4

  subroutine MAPL_BcastShared_2DR4(VM, Data, N, Root, RootOnly, rc)
    type(ESMF_VM) :: VM
    real,    pointer,  intent(INOUT) :: Data(:,:)
    integer,           intent(IN   ) :: N
    integer, optional, intent(IN   ) :: Root
    logical,           intent(IN   ) :: RootOnly
    integer, optional, intent(  OUT) :: rc


    integer :: status
    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_BcastShared'


    if(.not.MAPL_ShmInitialized) then
       if (RootOnly) then
          _RETURN(ESMF_SUCCESS)
       end if
       call MAPL_CommsBcast(vm, DATA=Data, N=N, ROOT=Root, RC=status)
       _RETURN(STATUS)
    else
       call MAPL_SyncSharedMemory(RC=STATUS)
       _VERIFY(STATUS)
       call MAPL_BroadcastToNodes(Data, N=N, ROOT=Root, rc=status)
       _VERIFY(STATUS)
       call MAPL_SyncSharedMemory(RC=STATUS)
       _VERIFY(STATUS)
    endif

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_BcastShared_2DR4

! Rank 0
!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "bcast.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 2
#include "bcast.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 3
#include "bcast.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 4
#include "bcast.H"

! Rank 1
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "bcast.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "bcast.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "bcast.H"

! Rank 2
!---------------------------
#define RANK_ 2
#define VARTYPE_ 1
#include "bcast.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "bcast.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "bcast.H"


!--AllReduceMin -----------------
    
! Rank 0
!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "allreducemin.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 3
#include "allreducemin.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 4
#include "allreducemin.H"

! Rank 1
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "allreducemin.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "allreducemin.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "allreducemin.H"

! Rank 2
!---------------------------
#define RANK_ 2
#define VARTYPE_ 1
#include "allreducemin.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "allreducemin.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "allreducemin.H"

!--AllReduceMax -----------------

! Rank 0
!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "allreducemax.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 3
#include "allreducemax.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 4
#include "allreducemax.H"

! Rank 1
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "allreducemax.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "allreducemax.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "allreducemax.H"

! Rank 2
!---------------------------
#define RANK_ 2
#define VARTYPE_ 1
#include "allreducemax.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "allreducemax.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "allreducemax.H"

!--AllReduceSum -----------------

! Rank 0
!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "allreducesum.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 3
#include "allreducesum.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 4
#include "allreducesum.H"

! Rank 1
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "allreducesum.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "allreducesum.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "allreducesum.H"

! Rank 2
!---------------------------
#define RANK_ 2
#define VARTYPE_ 1
#include "allreducesum.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "allreducesum.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "allreducesum.H"


! Scatter

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "scatter.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "scatter.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "scatter.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "scatter.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "scatter.H"

!---------------------------
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "gather.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "gather.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "gather.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "gather.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "gather.H"

!---------------------------
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "allgather.H"

!---------------------------
!---------------------------
#define RANK_ 1
#define VARTYPE_ 2
#include "allgather.H"

!---------------------------
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "allgatherv.H"

!---------------------------
!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "allgatherv.H"

!---------------------------
!---------------------------
#define RANK_ 1
#define VARTYPE_ 4 
#include "allgatherv.H"

!---------------------------
!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "send.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "send.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "send.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "send.H"


!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "send.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "send.H"

!---------------------------
!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "recv.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "recv.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "recv.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "recv.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "recv.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "recv.H"

!---------------------------
!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "sendrecv.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "sendrecv.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "sendrecv.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "sendrecv.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "sendrecv.H"

    subroutine MAPL_Abort
   
   integer :: status 
   call MPI_Abort(MPI_COMM_WORLD,status)
    end subroutine MAPL_Abort

!---------------------------
!---------------------------

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "arrayscatter.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "arrayscatter.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "arrayscatter.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "arrayscatter.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "arrayscatterRcvCnt.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "arrayscatterRcvCnt.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "arraygather.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "arraygather.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "arraygather.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "arraygather.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "arraygather.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "arraygatherRcvCnt.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "arraygatherRcvCnt.H"

!---------------------------
end module MAPL_CommsMod

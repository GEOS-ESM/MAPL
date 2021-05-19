#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

module MAPL_Shmem

  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64, REAL32

  implicit none
  private

  include 'mpif.h'

  public :: MAPL_GetNodeInfo
  public :: MAPL_CoresPerNodeGet
  public :: MAPL_InitializeShmem
  public :: MAPL_FinalizeShmem

  public :: MAPL_AllocNodeArray
  public :: MAPL_DeAllocNodeArray
  public :: MAPL_ShmemAmOnFirstNode
  public :: MAPL_SyncSharedMemory
  public :: MAPL_BroadcastToNodes

  public :: MAPL_AllocateShared
  public :: GetSharedMemory
  public :: ReleaseSharedMemory

  public :: MAPL_GetNewRank

  integer, public, parameter :: MAPL_NoShm=255

  character*30 :: Iam="MAPL_ShmemMod in line "

  integer(c_int), parameter :: IPC_CREAT = 512
  integer(c_int), parameter :: IPC_RMID  = 0
  integer,        parameter :: C_KEY_T = c_int32_t

  integer,        parameter :: CHUNK=256

  integer, public, save :: MAPL_NodeComm=-1
  integer, public, save :: MAPL_NodeRootsComm=-1
  integer, public, save :: MAPL_MyNodeNum=-1
  logical, public, save :: MAPL_AmNodeRoot=.false.
  logical, public, save :: MAPL_ShmInitialized=.false.

  integer,         save :: MAPL_CoresPerNodeUsed=-1
  integer,         save :: MAPL_CoresPerNodeMin=-1
  integer,         save :: MAPL_CoresPerNodeMax=-1
  integer,         save :: MAPL_NumNodes=-1

  type Segment_T
     integer (c_int) :: shmid=-1
     type    (c_ptr) :: addr
  end type Segment_T

  type(Segment_T), pointer :: Segs(:) => NULL()
  type(Segment_T), pointer :: SegsNew(:) => null()

  type NodeRankList_T
     integer, pointer :: rank(:) => NULL()
     integer          :: rankLastUsed
  end type NodeRankList_T

  type(NodeRankList_T), public, allocatable :: MAPL_NodeRankList(:)

  interface
     module function shmget(key, size, shmflg) bind(c, name="shmget")
       use, intrinsic :: ISO_C_BINDING
       implicit none
       integer (c_int)              :: shmget
       integer (c_key_t),     value :: key
       integer (c_size_t),    value :: size
       integer (c_int),       value :: shmflg
     end function shmget

     module function shmat(shmid, shmaddr, shmflg) bind(c, name="shmat")
       use, intrinsic :: ISO_C_BINDING
       implicit none
       type (c_ptr)           :: shmat
       integer (c_int), value :: shmid
       type (c_ptr),    value :: shmaddr
       integer (c_int), value :: shmflg
     end function shmat

     module function shmdt(shmaddr) bind(c, name="shmdt")
       use, intrinsic :: ISO_C_BINDING
       implicit none
       integer (c_int)     :: shmdt
       type (c_ptr), value :: shmaddr
     end function shmdt

     module function shmctl(shmid, cmd, buf) bind(c, name="shmctl")
       use, intrinsic :: ISO_C_BINDING
       implicit none
       integer (c_int)        :: shmctl
       integer (c_int), value :: shmid
       integer (c_int), value :: cmd
       type (c_ptr),    value :: buf
     end function shmctl

     subroutine perror(s) bind(c,name="perror")
       use, intrinsic :: ISO_C_BINDING
       character(c_char), intent(in) :: s(*)
     end subroutine perror

  end interface

  interface MAPL_AllocNodeArray
     module procedure MAPL_AllocNodeArray_1DL4
     module procedure MAPL_AllocNodeArray_1DI4
     module procedure MAPL_AllocNodeArray_2DI4
     module procedure MAPL_AllocNodeArray_3DI4
     module procedure MAPL_AllocNodeArray_4DI4
     module procedure MAPL_AllocNodeArray_1DR4
     module procedure MAPL_AllocNodeArray_2DR4
     module procedure MAPL_AllocNodeArray_3DR4
     module procedure MAPL_AllocNodeArray_4DR4
     module procedure MAPL_AllocNodeArray_1DR8
     module procedure MAPL_AllocNodeArray_2DR8
     module procedure MAPL_AllocNodeArray_3DR8
     module procedure MAPL_AllocNodeArray_4DR8
     module procedure MAPL_AllocNodeArray_5DR8
  end interface MAPL_AllocNodeArray

  interface MAPL_DeAllocNodeArray
     module procedure MAPL_DeAllocNodeArray_1DL4
     module procedure MAPL_DeAllocNodeArray_1DI4
     module procedure MAPL_DeAllocNodeArray_2DI4
     module procedure MAPL_DeAllocNodeArray_3DI4
     module procedure MAPL_DeAllocNodeArray_4DI4
     module procedure MAPL_DeAllocNodeArray_1DR4
     module procedure MAPL_DeAllocNodeArray_2DR4
     module procedure MAPL_DeAllocNodeArray_3DR4
     module procedure MAPL_DeAllocNodeArray_4DR4
     module procedure MAPL_DeAllocNodeArray_1DR8
     module procedure MAPL_DeAllocNodeArray_2DR8
     module procedure MAPL_DeAllocNodeArray_3DR8
     module procedure MAPL_DeAllocNodeArray_4DR8
     module procedure MAPL_DeAllocNodeArray_5DR8
  end interface MAPL_DeAllocNodeArray

  interface MAPL_BroadcastToNodes
     module procedure BroadcastToNodes_1DI4
     module procedure BroadcastToNodes_2DI4
     module procedure BroadcastToNodes_3DI4
     module procedure BroadcastToNodes_1DR4
     module procedure BroadcastToNodes_2DR4
     module procedure BroadcastToNodes_3DR4
     module procedure BroadcastToNodes_4DR4
     module procedure BroadcastToNodes_1DR8
     module procedure BroadcastToNodes_2DR8
     module procedure BroadcastToNodes_3DR8
     module procedure BroadcastToNodes_4DR8
  end interface MAPL_BroadcastToNodes

  interface MAPL_AllocateShared
     module procedure MAPL_AllocateShared_1DL4
     module procedure MAPL_AllocateShared_1DI4
     module procedure MAPL_AllocateShared_1DR4
     module procedure MAPL_AllocateShared_1DR8
     module procedure MAPL_AllocateShared_2DR4
     module procedure MAPL_AllocateShared_2DR8
  end interface MAPL_AllocateShared

  interface

     module subroutine MAPL_GetNodeInfo(comm, rc)
       integer,           intent(IN ) :: comm
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_GetNodeInfo

     module subroutine MAPL_InitializeShmem(rc)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_InitializeShmem

     module subroutine MAPL_FinalizeShmem(rc)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_FinalizeShmem

     module subroutine MAPL_DeAllocNodeArray_1DL4(Ptr,rc)
       logical,  pointer              :: Ptr(:)
       integer, optional, intent(OUT) :: rc

     end subroutine MAPL_DeAllocNodeArray_1DL4

     module subroutine MAPL_DeAllocNodeArray_1DI4(Ptr,rc)
       integer,  pointer              :: Ptr(:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_1DI4

     module subroutine MAPL_DeAllocNodeArray_2DI4(Ptr,rc)
       integer,  pointer              :: Ptr(:,:)
       integer, optional, intent(OUT) :: rc

     end subroutine MAPL_DeAllocNodeArray_2DI4

     module subroutine MAPL_DeAllocNodeArray_3DI4(Ptr,rc)
       integer,  pointer              :: Ptr(:,:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_3DI4

     module subroutine MAPL_DeAllocNodeArray_4DI4(Ptr,rc)
       integer,  pointer              :: Ptr(:,:,:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_4DI4


     module subroutine MAPL_DeAllocNodeArray_1DR4(Ptr,rc)
       real(kind=REAL32),  pointer    :: Ptr(:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_1DR4

     module subroutine MAPL_DeAllocNodeArray_2DR4(Ptr,rc)
       real(kind=REAL32),  pointer    :: Ptr(:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_2DR4

     module subroutine MAPL_DeAllocNodeArray_3DR4(Ptr,rc)
       real(kind=REAL32),  pointer    :: Ptr(:,:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_3DR4

     module subroutine MAPL_DeAllocNodeArray_4DR4(Ptr,rc)
       real,  pointer                 :: Ptr(:,:,:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_4DR4


     module subroutine MAPL_DeAllocNodeArray_1DR8(Ptr,rc)
       real(kind=REAL64),  pointer    :: Ptr(:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_1DR8

     module subroutine MAPL_DeAllocNodeArray_2DR8(Ptr,rc)
       real(kind=REAL64),  pointer    :: Ptr(:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_2DR8

     module subroutine MAPL_DeAllocNodeArray_3DR8(Ptr,rc)
       real(kind=REAL64),  pointer    :: Ptr(:,:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_3DR8

     module subroutine MAPL_DeAllocNodeArray_4DR8(Ptr,rc)
       real(kind=REAL64),  pointer    :: Ptr(:,:,:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_4DR8

     module subroutine MAPL_DeAllocNodeArray_5DR8(Ptr,rc)
       real(kind=REAL64),  pointer    :: Ptr(:,:,:,:,:)
       integer, optional, intent(OUT) :: rc
     end subroutine MAPL_DeAllocNodeArray_5DR8

     module subroutine MAPL_AllocNodeArray_1DL4(Ptr, Shp, lbd, rc)
       logical, pointer,  intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_1DL4

     module subroutine MAPL_AllocNodeArray_1DI4(Ptr, Shp, lbd, rc)
       integer, pointer,  intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_1DI4


     module subroutine MAPL_AllocNodeArray_2DI4(Ptr, Shp, lbd, rc)
       integer, pointer,  intent(INOUT) :: Ptr(:,:)
       integer,           intent(IN   ) :: Shp(2)
       integer, optional, intent(IN   ) :: lbd(2)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_2DI4

     module subroutine MAPL_AllocNodeArray_3DI4(Ptr, Shp, lbd, rc)
       integer, pointer,  intent(INOUT) :: Ptr(:,:,:)
       integer,           intent(IN   ) :: Shp(3)
       integer, optional, intent(IN   ) :: lbd(3)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_3DI4

     module subroutine MAPL_AllocNodeArray_4DI4(Ptr, Shp, lbd, rc)
       integer, pointer,  intent(INOUT) :: Ptr(:,:,:,:)
       integer,           intent(IN   ) :: Shp(4)
       integer, optional, intent(IN   ) :: lbd(4)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_4DI4

     module subroutine MAPL_AllocNodeArray_1DR4(Ptr, Shp, lbd, rc)
       real(kind=REAL32), pointer,   intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_1DR4


     module subroutine MAPL_AllocNodeArray_2DR4(Ptr, Shp, lbd, rc)
       real(kind=REAL32), pointer,   intent(INOUT) :: Ptr(:,:)
       integer,           intent(IN   ) :: Shp(2)
       integer, optional, intent(IN   ) :: lbd(2)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_2DR4

     module subroutine MAPL_AllocNodeArray_3DR4(Ptr, Shp, lbd, rc)
       real(kind=REAL32), pointer,   intent(INOUT) :: Ptr(:,:,:)
       integer,           intent(IN   ) :: Shp(3)
       integer, optional, intent(IN   ) :: lbd(3)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_3DR4

     module subroutine MAPL_AllocNodeArray_4DR4(Ptr, Shp, lbd, rc)
       real, pointer,     intent(INOUT) :: Ptr(:,:,:,:)
       integer,           intent(IN   ) :: Shp(4)
       integer, optional, intent(IN   ) :: lbd(4)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_4DR4


     module subroutine MAPL_AllocNodeArray_1DR8(Ptr, Shp, lbd, rc)
       real(kind=REAL64), pointer,   intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_1DR8


     module subroutine MAPL_AllocNodeArray_2DR8(Ptr, Shp, lbd, rc)
       real(kind=REAL64), pointer,   intent(INOUT) :: Ptr(:,:)
       integer,           intent(IN   ) :: Shp(2)
       integer, optional, intent(IN   ) :: lbd(2)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_2DR8

     module subroutine MAPL_AllocNodeArray_3DR8(Ptr, Shp, lbd, rc)
       real(kind=REAL64), pointer,   intent(INOUT) :: Ptr(:,:,:)
       integer,           intent(IN   ) :: Shp(3)
       integer, optional, intent(IN   ) :: lbd(3)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_3DR8

     module subroutine MAPL_AllocNodeArray_4DR8(Ptr, Shp, lbd, rc)
       real(kind=REAL64), pointer,   intent(INOUT) :: Ptr(:,:,:,:)
       integer,           intent(IN   ) :: Shp(4)
       integer, optional, intent(IN   ) :: lbd(4)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_4DR8

     module subroutine MAPL_AllocNodeArray_5DR8(Ptr, Shp, lbd, rc)
       real(kind=REAL64), pointer,   intent(INOUT) :: Ptr(:,:,:,:,:)
       integer,           intent(IN   ) :: Shp(5)
       integer, optional, intent(IN   ) :: lbd(5)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocNodeArray_5DR8


     module subroutine MAPL_AllocateShared_1DL4(Ptr, Shp, lbd, TransRoot, rc)
       logical, pointer,  intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       logical,           intent(IN   ) :: TransRoot
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocateShared_1DL4

     module subroutine MAPL_AllocateShared_1DI4(Ptr, Shp, lbd, TransRoot, rc)
       integer, pointer,  intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       logical,           intent(IN   ) :: TransRoot
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocateShared_1DI4

     module subroutine MAPL_AllocateShared_1DR4(Ptr, Shp, lbd, TransRoot, rc)
       real, pointer,     intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       logical,           intent(IN   ) :: TransRoot
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocateShared_1DR4

     module subroutine MAPL_AllocateShared_1DR8(Ptr, Shp, lbd, TransRoot, rc)
       real(KIND=REAL64), pointer,     intent(INOUT) :: Ptr(:)
       integer,           intent(IN   ) :: Shp(1)
       integer, optional, intent(IN   ) :: lbd(1)
       logical,           intent(IN   ) :: TransRoot
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocateShared_1DR8

     module subroutine MAPL_AllocateShared_2DR4(Ptr, Shp, lbd, TransRoot, rc)
       real,    pointer,  intent(INOUT) :: Ptr(:,:)
       integer,           intent(IN   ) :: Shp(2)
       integer, optional, intent(IN   ) :: lbd(2)
       logical,           intent(IN   ) :: TransRoot
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocateShared_2DR4

     module subroutine MAPL_AllocateShared_2DR8(Ptr, Shp, lbd, TransRoot, rc)
       real(KIND=REAL64), pointer, intent(INOUT) :: Ptr(:,:)
       integer,           intent(IN   ) :: Shp(2)
       integer, optional, intent(IN   ) :: lbd(2)
       logical,           intent(IN   ) :: TransRoot
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocateShared_2DR8

     module subroutine ReleaseSharedMemory(Caddr,rc)
       type(c_ptr),       intent(INOUT) :: Caddr
       integer, optional, intent(  OUT) :: rc
     end subroutine ReleaseSharedMemory



     module subroutine GetSharedMemory(Caddr,Len,rc)
       type(c_ptr),       intent(  OUT) :: Caddr
       integer,           intent(IN   ) :: Len
       integer, optional, intent(  OUT) :: rc
     end subroutine GetSharedMemory

     module subroutine BroadcastToNodes_1DR4(DATA,N,ROOT,rc)
       real(kind=REAL32), intent(INOUT) :: DATA(:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_1DR4

     module subroutine BroadcastToNodes_2DR4(DATA,N,ROOT,rc)
       real(kind=REAL32), intent(INOUT) :: DATA(:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_2DR4

     module subroutine BroadcastToNodes_3DR4(DATA,N,ROOT,rc)
       real,              intent(INOUT) :: DATA(:,:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_3DR4

     module subroutine BroadcastToNodes_4DR4(DATA,N,ROOT,rc)
       real,              intent(INOUT) :: DATA(:,:,:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_4DR4

     module subroutine BroadcastToNodes_1DR8(DATA,N,ROOT,rc)
       real(kind=REAL64), intent(INOUT) :: DATA(:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_1DR8

     module subroutine BroadcastToNodes_2DR8(DATA,N,ROOT,rc)
       real(kind=REAL64), intent(INOUT) :: DATA(:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_2DR8

     module subroutine BroadcastToNodes_3DR8(DATA,N,ROOT,rc)
       real(kind=REAL64), intent(INOUT) :: DATA(:,:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_3DR8

     module subroutine BroadcastToNodes_4DR8(DATA,N,ROOT,rc)
       real(kind=REAL64), intent(INOUT) :: DATA(:,:,:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_4DR8

     module subroutine BroadcastToNodes_1DI4(DATA,N,ROOT,rc)
       integer,           intent(INOUT) :: DATA(:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_1DI4

     module subroutine BroadcastToNodes_2DI4(DATA,N,ROOT,rc)
       integer,           intent(INOUT) :: DATA(:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_2DI4

     module subroutine BroadcastToNodes_3DI4(DATA,N,ROOT,rc)
       integer,           intent(INOUT) :: DATA(:,:,:)
       integer,           intent(IN   ) :: N
       integer,           intent(IN   ) :: ROOT
       integer, optional, intent(  OUT) :: rc
     end subroutine BroadcastToNodes_3DI4

     module subroutine MAPL_SyncSharedMemory(rc)
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_SyncSharedMemory

     module function MAPL_GetNewRank(node,rc) result(rank)
       integer :: rank
       integer, intent(in) :: node
       integer, optional, intent(out) :: rc
     end function MAPL_GetNewRank

     module function getNodeComm(Comm, rc) result(NodeComm)
       integer,           intent( IN) :: Comm
       integer, optional, intent(OUT) :: rc
       integer                        :: NodeComm
     end function getNodeComm

     module function getNodeRootsComm(Comm, rc) result(NodeRootsComm)
       integer,           intent( IN) :: Comm
       integer, optional, intent(OUT) :: rc
       integer                        :: NodeRootsComm
     end function getNodeRootsComm


     module function MAPL_ShmemAmOnFirstNode(comm, rc) result(a)
       integer,           intent(IN   ) :: comm
       integer, optional, intent(  OUT) :: RC
       logical                          :: a
     end function MAPL_ShmemAmOnFirstNode

     integer module function MAPL_CoresPerNodeGet(comm, rc)
       integer,           intent(IN   ) :: comm
       integer, optional, intent(  OUT) :: RC
     end function MAPL_CoresPerNodeGet
  end interface
end module MAPL_Shmem

! For backwards compatibility
module MAPL_ShmemMod
  use MAPL_Shmem
end module MAPL_ShmemMod

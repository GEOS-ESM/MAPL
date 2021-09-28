!This file should include contents of FileIOShared and be renamed to
!FileIOShared. Current FileIOShared will disappear.
!NCIO and BinIO would then use this FileIOShared
!A new MAPL_IO.F90 would then only contain use NCIO and BinIO.

#include "MAPL_ErrLog.h"

#define DEALOC_(A) if(associated(A))then;if(MAPL_ShmInitialized)then;call MAPL_SyncSharedMemory(rc=STATUS);call MAPL_DeAllocNodeArray(A,rc=STATUS);else;deallocate(A,stat=STATUS);endif;_VERIFY(STATUS);NULLIFY(A);endif


!BOP

! !MODULE: FileIO_Shared -- A Module that contains shared subroutines/functions needed by NetCDF and Binary IO


! !INTERFACE:

module FileIOSharedMod

  use ESMF
  use MAPL_BaseMod
  use MAPL_SortMod
  use MAPL_CommsMod
  use MAPL_ShmemMod
  use MAPL_ExceptionHandling
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  ! public types
  public ArrDescr

  ! public interfaces
  public WRITE_PARALLEL

  ! public subroutines
  public MAPL_TileMaskGet
  public alloc_
  public dealloc_

  include "mpif.h"

! Global vars:
! ------------

  integer, parameter, public :: STD_OUT_UNIT_NUMBER = 6   !shared
  integer, parameter, public :: LAST_UNIT = 999
  logical, save, public      :: TAKEN(LAST_UNIT)=.FALSE.  !shared
  logical, save, public      :: MTAKEN(LAST_UNIT)=.FALSE. !shared
  character(len=ESMF_MAXSTR), save, public  :: mname(LAST_UNIT)
  integer, parameter :: UNDEF = 999

  integer, parameter, public :: not_allocated = 0     !all shared
  integer, parameter, public :: r4_2 = 1
  integer, parameter, public :: r4_1 = 2
  integer, parameter, public :: r8_2 = 3
  integer, parameter, public :: r8_1 = 4
  integer, parameter, public :: i4_2 = 5
  integer, parameter, public :: i4_1 = 6

  type PTR   !shared
   integer :: allocated=not_allocated
   real(kind=ESMF_KIND_R4)   , pointer :: r4_2(:,:) => null()
   real(kind=ESMF_KIND_R4)   , pointer :: r4_1(:) => null()
   real(kind=ESMF_KIND_R4)             :: r4_0
   real(kind=ESMF_KIND_R8)   , pointer :: r8_2(:,:) => null()
   real(kind=ESMF_KIND_R8)   , pointer :: r8_1(:) => null()
   real(kind=ESMF_KIND_R8)             :: r8_0
   integer(kind=ESMF_KIND_I4), pointer :: I4_2(:,:) => null()
   integer(kind=ESMF_KIND_I4), pointer :: I4_1(:) => null()
   integer(kind=ESMF_KIND_I4)          :: I4_0
  end type PTR

  type memunit !shared
     integer :: prevrec = 0
     type (PTR), pointer :: Records(:)=>null()
  end type MEMUNIT

  !shared
  type (memunit), target, save, public :: MEM_UNITS(LAST_UNIT)
  type (memunit), pointer, public      :: munit
  type(PTR), pointer, public           :: REC(:)

  type ArrDescr
     integer(kind=MPI_OFFSET_KIND) :: offset
     character(len=MPI_MAX_INFO_VAL) :: romio_cb_read,cb_buffer_size,romio_cb_write
     integer :: Xcomm, Ycomm, NX0, NY0
     integer :: readers_comm, IOscattercomm
     integer :: writers_comm, IOgathercomm
     integer :: face_writers_comm
     integer :: face_readers_comm
     integer :: face_index
     logical :: write_restart_by_face = .false.
     logical :: read_restart_by_face = .false.
     integer, pointer :: i1(:), in(:), j1(:), jn(:)
     integer :: im_world, jm_world, lm_world
     type (ESMF_Grid) :: grid
     logical :: tile
     integer :: num_readers = 1
     integer :: num_writers = 1
     ! only used when writing though o_server
     logical :: write_restart_by_oserver = .false.
     integer :: collection_id = -1
     character(LEN=ESMF_MAXSTR) :: filename
  end type ArrDescr


  !Binary and NetCDF interfaces
  interface WRITE_PARALLEL
     module procedure WRITE_PARALLEL_I4_0
     module procedure WRITE_PARALLEL_I4_1
     module procedure WRITE_PARALLEL_R4_0
     module procedure WRITE_PARALLEL_R4_1
     module procedure WRITE_PARALLEL_R8_0
     module procedure WRITE_PARALLEL_R8_1
     module procedure WRITE_PARALLEL_STRING_0
  end interface

  contains

!--WRITES ------------------

!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "write_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "write_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 3
#include "write_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "write_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 4
#include "write_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "write_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 0
#include "write_parallel.H"

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

  subroutine alloc_(A,type,im,jm,rc)
    type (Ptr),        intent(INOUT) :: A
    integer,           intent(IN)    :: TYPE
    integer,           intent(IN)    :: IM
    integer, optional, intent(IN)    :: JM
    integer, optional, intent(out)   :: rc

    integer :: status

    call dealloc_(A,RC=STATUS)
    _VERIFY(STATUS)

    select case (type)
    case (R4_2)
       _ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%r4_2(IM,JM))
    case (R4_1)
       _ASSERT(.not.present(jm), 'jm is present for 1d')
       allocate(A%r4_1(IM))
    case (R8_2)
       _ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%r8_2(IM,JM))
    case (R8_1)
       _ASSERT(.not.present(jm),'jm is present for 1d')
       allocate(A%r8_1(IM))
    case (i4_1)
       _ASSERT(.not.present(jm), 'jm present for 1d')
       allocate(A%I4_1(IM))
    case (i4_2)
       _ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%I4_2(IM,JM))
    case default
       _ASSERT(.false., 'unsupported tkr')
    end select

    a%allocated=type

    _RETURN(ESMF_SUCCESS)
  end subroutine alloc_

  subroutine dealloc_(A,RC)
    type (Ptr), intent(INOUT)        :: A
    integer, optional, intent(out)   :: rc


    if(a%allocated/=not_allocated) then
       select case (a%allocated)
       case (R4_2)
          if(associated(A%r4_2)) then
             deallocate(A%r4_2)
             nullify(A%r4_2)
          end if
       case (R4_1)
          if(associated(A%r4_1)) then
             deallocate(A%r4_1)
             nullify(A%r4_1)
          end if
       case (R8_2)
          if(associated(A%r8_2)) then
             deallocate(A%r8_2)
             nullify(A%r8_2)
          end if
       case (R8_1)
          if(associated(A%r8_1)) then
             deallocate(A%r8_1)
             nullify(A%r8_1)
          end if
       case (i4_1)
          if(associated(A%i4_1)) then
             deallocate(A%i4_1)
             nullify(A%i4_1)
          end if
       case (i4_2)
          if(associated(A%i4_2)) then
             deallocate(A%i4_2)
             nullify(A%i4_2)
          end if
       case default
          _ASSERT(.false., 'unsupported tkr')
       end select
       a%allocated=not_allocated
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine dealloc_

  subroutine MAPL_TileMaskGet(grid, mask, rc)
    type (ESMF_Grid),             intent(INout) :: GRID
    integer, pointer                            :: mask(:)
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: STATUS
    integer, pointer                      :: tileIndex(:)
    integer                               :: gcount(2), lcount(2)
    integer                               :: gsize, lsize
    integer                               :: gridRank
    integer                               :: n
    type (ESMF_DistGrid)                  :: distGrid

    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer, allocatable, dimension(:)    :: recvcounts, displs
    integer                               :: de, deId
    integer                               :: nDEs
    integer                               :: sendcount

    integer                               :: I
    integer                               :: I1, IN
    integer, allocatable                  :: var(:)
    type (ESMF_DELayout)                  :: layout

    type(ESMF_VM) :: vm
    logical :: amIRoot

    call ESMF_GridGet(grid, dimCount=gridRank, distGrid=distGrid, rc=status)
    _VERIFY(STATUS)
    _ASSERT(gridRank == 1, 'gridRank must be 1')

    call MAPL_GridGet(grid, globalCellCountPerDim=gcount, &
         localCellCountPerDim=lcount, RC=STATUS)
    _VERIFY(STATUS)

    gsize = gcount(1)
    lsize = lcount(1)

    call ESMF_DistGridGet(distgrid, localDe=0, elementCount=n, rc=status)
    _ASSERT(lsize == n, ' inconsistent lsize')

    allocate(tileIndex(lsize), stat=status)
    _VERIFY(STATUS)

    call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=tileIndex, rc=status)
    _VERIFY(STATUS)

    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    _VERIFY(STATUS)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, rc=status)
    _VERIFY(STATUS)

    amIRoot = MAPL_AM_I_Root(vm)

    call ESMF_VmBarrier(vm, rc=status)
    _VERIFY(STATUS)

    if (.not. MAPL_ShmInitialized) then
       allocate(mask(gsize), stat=status)
       _VERIFY(STATUS)
    else
       call MAPL_AllocNodeArray(mask,(/gsize/),rc=STATUS)
       _VERIFY(STATUS)
    end if

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)

    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, rc=status)
    _VERIFY(STATUS)

    allocate (recvcounts(0:nDEs-1), displs(0:nDEs), stat=status)
    _VERIFY(STATUS)

    if (.not. MAPL_ShmInitialized .or. amIRoot) then
       allocate(VAR(0:gsize-1), stat=status)
       _VERIFY(STATUS)
    else
       allocate(VAR(0), stat=status)
       _VERIFY(STATUS)
    end if

    displs(0) = 0
    do I = 0,nDEs-1
       de = I
       I1 = AL(1,I)
       IN = AU(1,I)

       recvcounts(I) = (IN - I1 + 1)
       if (de == deId) then
          sendcount = recvcounts(I)      ! Count I will send
       endif

       displs(I+1) = displs(I) + recvcounts(I)
    enddo

#ifdef NEW
    _ASSERT(.false., 'unsupported code block') !ALT this section is questionable
    do I = 0,nDEs-1
       de = I
       I1 = AL(1,I)
       IN = AU(1,I)
       var(I1:IN) = -9999
       if (de == deId) then
          var(I1:IN) = tileindex
       endif
       do II=I1,IN
          mmax=var(II)
          call MAPL_CommsAllReduceMax(vm, mmax, var(II), 1, rc=status)
          _VERIFY(STATUS)
       enddo
    end do
#else
    if (MAPL_ShmInitialized) then
       call MAPL_CommsGatherV(layout, tileindex, sendcount, &
                              var, recvcounts, displs, MAPL_Root, status)
       _VERIFY(STATUS)
    else
       call MAPL_CommsAllGatherV(layout, tileindex, sendcount, &
                                 var, recvcounts, displs, status)
       _VERIFY(STATUS)
    endif
#endif

    if (.not. MAPL_ShmInitialized .or. amIRoot) then
       do I = 0,nDEs-1
          mask(displs(I)+1:displs(I+1)) = I
       end do
       call MAPL_SORT(var,MASK)
    end if

! clean up

    deallocate(var)
    deallocate (recvcounts, displs)
    deallocate (AU)
    deallocate (AL)
    deallocate(tileIndex)

! mask is deallocated in the caller routine
       call MAPL_BroadcastToNodes(MASK, N=gsize, ROOT=MAPL_Root, rc=status)
       _VERIFY(STATUS)

    call MAPL_SyncSharedMemory(rc=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_TileMaskGet

end module FileIOSharedMod

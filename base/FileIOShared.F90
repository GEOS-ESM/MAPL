!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_ErrLog.h"

#define DEALOC_(A) if(associated(A))then;if(MAPL_ShmInitialized)then;call MAPL_SyncSharedMemory(rc=STATUS);call MAPL_DeAllocNodeArray(A,rc=STATUS);else;deallocate(A,stat=STATUS);endif;__VERIFY(STATUS);NULLIFY(A);endif
!
!>
!### MODULE: `FileIO_Shared`
!
! Author: GMAO SI-Team
!
! `FileIO_Shared` -- A Module that contains shared subroutines/functions needed by NetCDF and Binary IO
!
module FileIOSharedMod

  use ESMF
  use MAPL_BaseMod
  use MAPL_SortMod
  use MAPL_CommsMod
  use MAPL_ShmemMod
  use MAPL_ExceptionHandling
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env
  use mpi
  implicit none
  private

  ! public types
  public ArrDescr

  ! public interfaces
  public WRITE_PARALLEL
  public ArrayScatterShm

  ! public subroutines
  public MAPL_TileMaskGet
  public alloc_
  public dealloc_
  public ArrDescrSet
  public ArrDescrInit
  public ArrDescrCreateReaderComm
  public ArrDescrCreateWriterComm
  public ArrDescrCommFree

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
     integer :: NX0, NY0
     integer :: Xcomm = MPI_COMM_NULL
     integer :: Ycomm = MPI_COMM_NULL
     integer :: readers_comm = MPI_COMM_NULL
     integer :: IOscattercomm = MPI_COMM_NULL
     integer :: writers_comm = MPI_COMM_NULL
     integer :: IOgathercomm = MPI_COMM_NULL
     integer :: myrow
     logical :: split_restart = .false.
     logical :: split_checkpoint = .false.
     integer, pointer :: i1(:), in(:), j1(:), jn(:)
     integer :: im_world, jm_world, lm_world
     type (ESMF_Grid) :: grid
     logical :: tile
     integer :: num_readers = 1
     integer :: num_writers = 1
     ! only used when writing though o_server
     logical :: write_restart_by_oserver = .false.
     integer, allocatable :: collection_id(:)
     character(LEN=ESMF_MAXSTR) :: filename
     integer :: writer_id
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

  interface ArrayScatterShm
     module procedure ArrayScatterShmR4D1
  end interface ArrayScatterShm

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
    __VERIFY(STATUS)

    select case (type)
    case (R4_2)
       __ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%r4_2(IM,JM))
    case (R4_1)
       __ASSERT(.not.present(jm), 'jm is present for 1d')
       allocate(A%r4_1(IM))
    case (R8_2)
       __ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%r8_2(IM,JM))
    case (R8_1)
       __ASSERT(.not.present(jm),'jm is present for 1d')
       allocate(A%r8_1(IM))
    case (i4_1)
       __ASSERT(.not.present(jm), 'jm present for 1d')
       allocate(A%I4_1(IM))
    case (i4_2)
       __ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%I4_2(IM,JM))
    case default
       __FAIL( 'unsupported tkr')
    end select

    a%allocated=type

    __RETURN(ESMF_SUCCESS)
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
          __FAIL( 'unsupported tkr')
       end select
       a%allocated=not_allocated
    end if

    __RETURN(ESMF_SUCCESS)
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

    call ESMF_GridGet(grid, dimCount=gridRank, distGrid=distGrid, __RC)
    __ASSERT(gridRank == 1, 'gridRank must be 1')

    call MAPL_GridGet(grid, globalCellCountPerDim=gcount, &
         localCellCountPerDim=lcount, __RC)

    gsize = gcount(1)
    lsize = lcount(1)

    call ESMF_DistGridGet(distgrid, localDe=0, elementCount=n, rc=status)
    __ASSERT(lsize == n, ' inconsistent lsize')

    allocate(tileIndex(lsize), __STAT)

    call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=tileIndex, __RC)

    call ESMF_DistGridGet(distGRID, delayout=layout, __RC)
    call ESMF_DELayoutGet(layout, vm=vm, __RC)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, __RC)

    amIRoot = MAPL_AM_I_Root(vm)

    call ESMF_VmBarrier(vm, __RC)

    if (.not. MAPL_ShmInitialized) then
       allocate(mask(gsize), __STAT)
    else
       call MAPL_AllocNodeArray(mask,(/gsize/),__RC)
    end if

    allocate (AL(gridRank,0:nDEs-1),  __STAT)
    allocate (AU(gridRank,0:nDEs-1),  __STAT)

    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, __RC)

    allocate (recvcounts(0:nDEs-1), displs(0:nDEs), __STAT)

    if (.not. MAPL_ShmInitialized .or. amIRoot) then
       allocate(VAR(0:gsize-1), __STAT)
    else
       allocate(VAR(0), __STAT)
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
    __FAIL( 'unsupported code block') !ALT this section is questionable
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
          call MAPL_CommsAllReduceMax(vm, mmax, var(II), 1, __RC)
       enddo
    end do
#else
    if (MAPL_ShmInitialized) then
       call MAPL_CommsGatherV(layout, tileindex, sendcount, &
                              var, recvcounts, displs, MAPL_Root, __RC)
    else
       call MAPL_CommsAllGatherV(layout, tileindex, sendcount, &
                                 var, recvcounts, displs, __RC)
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
       call MAPL_BroadcastToNodes(MASK, N=gsize, ROOT=MAPL_Root, __RC)

    call MAPL_SyncSharedMemory(__RC)

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_TileMaskGet

      subroutine ArrDescrInit(ArrDes,comm,im_world,jm_world,lm_world,nx,ny,num_readers,num_writers,is,ie,js,je,rc)
         type(ArrDescr), intent(INOUT) :: ArrDes
         integer, intent(in) :: comm
         integer, intent(in) :: IM_World
         integer, intent(in) :: JM_World
         integer, intent(in) :: lm_world
         integer, intent(in) :: nx
         integer, intent(in) :: ny
         integer, intent(in) :: num_readers
         integer, intent(in) :: num_writers
         integer, intent(in) :: is
         integer, intent(in) :: ie
         integer, intent(in) :: js
         integer, intent(in) :: je
         integer, optional, intent(out) :: rc

         integer :: color,myid,npes,NX0,NY0,ny_by_readers,ny_by_writers,j
         integer :: readers_comm, writers_comm, ioscattercomm,iogathercomm, xcomm,ycomm
         integer, allocatable :: i1(:),in(:),j1(:),jn(:)
         integer, allocatable :: iminw(:),imaxw(:),jminw(:),jmaxw(:)
         integer :: imincnt,jmincnt,imaxcnt,jmaxcnt,i

         integer :: status

         call MPI_Comm_Rank(comm,myid,status)
         __VERIFY(status)
         call MPI_COMM_Size(comm,npes,status)
         __VERIFY(status)

         allocate(iminw(npes),imaxw(npes),jminw(npes),jmaxw(npes),stat=status)
         iminw=-1
         imaxw=-1
         jminw=-1
         jmaxw=-1
         iminw(myid+1)=is
         imaxw(myid+1)=ie
         jminw(myid+1)=js
         jmaxw(myid+1)=je
         call MPI_AllReduce(MPI_IN_PLACE,iminw,npes,MPI_INTEGER,MPI_MAX,comm,status)
         __VERIFY(STATUS)
         call MPI_AllReduce(MPI_IN_PLACE,imaxw,npes,MPI_INTEGER,MPI_MAX,comm,status)
         __VERIFY(STATUS)
         call MPI_AllReduce(MPI_IN_PLACE,jminw,npes,MPI_INTEGER,MPI_MAX,comm,status)
         __VERIFY(STATUS)
         call MPI_AllReduce(MPI_IN_PLACE,jmaxw,npes,MPI_INTEGER,MPI_MAX,comm,status)
         __VERIFY(STATUS)

         call MAPL_Sort(iminw)
         call MAPL_Sort(imaxw)
         call MAPL_Sort(jminw)
         call MAPL_Sort(jmaxw)

         allocate(i1(nx),in(nx),j1(ny),jn(ny))
         i1(1) = minval(iminw)
         in(1) = minval(imaxw)
         j1(1) = minval(jminw)
         jn(1) = minval(jmaxw)
         imincnt = 1
         imaxcnt = 1
         jmincnt = 1
         jmaxcnt = 1
         do i=1,npes
            if (iminw(i) > i1(imincnt)) then
               imincnt = imincnt + 1
               i1(imincnt) = iminw(i)
            end if
            if (imaxw(i) > in(imaxcnt)) then
               imaxcnt = imaxcnt + 1
               in(imaxcnt) = imaxw(i)
            end if
            if (jminw(i) > j1(jmincnt)) then
               jmincnt = jmincnt + 1
               j1(jmincnt) = jminw(i)
            end if
            if (jmaxw(i) > jn(jmaxcnt)) then
               jmaxcnt = jmaxcnt + 1
               jn(jmincnt) = jmaxw(i)
            end if
         enddo
         deallocate(iminw,imaxw,jminw,jmaxw)

         NX0 = mod(myid,nx) + 1
         NY0 = myid/nx + 1
         color = nx0
         call MPI_Comm_Split(comm,color,myid,ycomm,status)
         __VERIFY(status)
         color = ny0
         call MPI_Comm_Split(comm,color,myid,xcomm,status)
         __VERIFY(status)
         ! reader communicators
         if (num_readers > ny .or. mod(ny,num_readers) /= 0) then
            __RETURN(ESMF_FAILURE)
         end if
         ny_by_readers = ny/num_readers
         if (mod(myid,nx*ny/num_readers) ==0) then
            color = 0
         else
            color = MPI_UNDEFINED
         end if
         call MPI_COMM_SPLIT(comm,color,myid,readers_comm,status)
         __VERIFY(status)
         if (num_readers==ny) then
            IOscattercomm = xcomm
         else
            j = ny0 - mod(ny0-1,ny_by_readers)
            call MPI_Comm_Split(comm,j,myid,IOScattercomm,status)
            __VERIFY(status)
         endif
         ! writer communicators
         if (num_writers > ny .or. mod(ny,num_writers) /= 0) then
            __RETURN(ESMF_FAILURE)
         end if
         ny_by_writers = ny/num_writers
         if (mod(myid,nx*ny/num_writers) ==0) then
            color = 0
         else
            color = MPI_UNDEFINED
         end if
         call MPI_COMM_SPLIT(comm,color,myid,writers_comm,status)
         __VERIFY(status)
         if (num_writers==ny) then
            IOgathercomm = xcomm
         else
            j = ny0 - mod(ny0-1,ny_by_writers)
            call MPI_Comm_Split(comm,j,myid,IOgathercomm,status)
            __VERIFY(status)
         endif

         ArrDes%im_world=im_world
         ArrDes%jm_world=jm_world
         ArrDes%lm_world=lm_world

         call MAPL_Comm_dup(readers_comm, ArrDes%readers_comm, status)
         call MAPL_Comm_dup(ioscattercomm, ArrDes%IOscattercomm, status)
         call MAPL_Comm_dup(writers_comm, ArrDes%writers_comm, status)
         call MAPL_Comm_dup(iogathercomm, ArrDes%IOgathercomm, status)
         call MAPL_Comm_dup(xcomm, ArrDes%Xcomm, status)
         call MAPL_Comm_dup(ycomm, ArrDes%Ycomm, status)
         call mpi_comm_rank(arrdes%ycomm,arrdes%myrow,status)
         __VERIFY(status)

         allocate(arrdes%i1(size(i1)),__STAT)
         arrdes%i1=i1
         allocate(arrdes%in(size(in)),__STAT)
         arrdes%in=in
         allocate(arrdes%j1(size(j1)),__STAT)
         arrdes%j1=j1
         allocate(arrdes%jn(size(jn)),__STAT)
         arrdes%jn=jn

         ArrDes%NX0 = NY0
         ArrDes%NY0 = NX0

         ArrDes%offset = 0

         ArrDes%romio_cb_read  = "automatic"
         ArrDes%cb_buffer_size = "16777216"
         ArrDes%romio_cb_write = "enable"

         ArrDes%tile = .false.

         ArrDes%filename = ''

         __RETURN(ESMF_SUCCESS)

      end subroutine ArrDescrInit

    subroutine ArrDescrSet(ArrDes, offset, &
         readers_comm, ioscattercomm, &
         writers_comm, iogathercomm, &
         i1, in, j1, jn, im_world, jm_world, lm_world)

      type(ArrDescr),                 intent(INOUT) :: ArrDes
      integer(kind=MPI_OFFSET_KIND), &
                           optional,  intent(IN   ) :: offset
      integer, optional,              intent(IN   ) :: readers_comm, ioscattercomm
      integer, optional,              intent(IN   ) :: writers_comm, iogathercomm
      integer, optional, target                    :: i1(:), in(:), j1(:), jn(:)
      integer, optional,              intent(IN   ) :: im_world, jm_world, lm_world
      integer :: status
      
      if(present(offset  )) ArrDes%offset   = offset
      if(present(readers_comm )) then
         call MAPL_Comm_dup(readers_comm, ArrDes%readers_comm, status)
      end if
      if(present(ioscattercomm)) then
         call MAPL_Comm_dup(IOscattercomm, ArrDes%IOscattercomm, status)
      end if
      if(present(writers_comm )) then
         call MAPL_Comm_dup(writers_comm, ArrDes%writers_comm, status)
      end if
      if(present(iogathercomm)) then
         call MAPL_Comm_dup(IOgathercomm, ArrDes%IOgathercomm, status)
      end if
      if(present(i1      )) ArrDes%i1       => i1
      if(present(in      )) ArrDes%in       => in
      if(present(j1      )) ArrDes%j1       => j1
      if(present(jn      )) ArrDes%jn       => jn
      if(present(im_world)) ArrDes%im_world = im_world
      if(present(jm_world)) ArrDes%jm_world = jm_world
      if(present(lm_world)) ArrDes%lm_world = lm_world

    end subroutine ArrDescrSet

    subroutine ArrDescrCreateWriterComm(arrdes, full_comm, num_writers, rc)
       type(ArrDescr), intent(inout) :: arrdes
       integer, intent(in) :: full_comm
       integer, intent(in) :: num_writers
       integer, optional, intent(out) :: rc

       integer :: status, nx, ny, color, ny_by_writers, myid, j, writer_rank

       nx = size(arrdes%i1)
       ny = size(arrdes%j1)
       __ASSERT(num_writers <= ny,'num writers must be less or equal to than NY')
       __ASSERT(mod(ny,num_writers)==0,'num writerss must evenly divide NY')
       call mpi_comm_rank(full_comm,myid, status)
       __VERIFY(status)
       color =  arrdes%NX0
       call MPI_COMM_SPLIT(full_comm, color, MYID, arrdes%Ycomm, status)
       __VERIFY(status)
       color = arrdes%NY0
       call MPI_COMM_SPLIT(full_comm, color, MYID, arrdes%Xcomm, status)
       __VERIFY(status)
       ny_by_writers = ny/num_writers
       if (mod(myid,nx*ny/num_writers) == 0) then
          color = 0
       else
          color = MPI_UNDEFINED
       endif
       call MPI_COMM_SPLIT(full_comm, color, myid, arrdes%writers_comm, status)
       __VERIFY(status)
       if (num_writers==ny) then
          call MAPL_Comm_dup(arrdes%Xcomm, ArrDes%IOgathercomm, status)
       else
            j = arrdes%NY0 - mod(arrdes%NY0-1,ny_by_writers)
          call MPI_COMM_SPLIT(full_comm, j, myid, arrdes%IOgathercomm, status)
          __VERIFY(status)
       endif
       if (arrdes%writers_comm /= MPI_COMM_NULL) then
          call mpi_comm_rank(arrdes%writers_comm,writer_rank,status)
          __VERIFY(STATUS)
       end if
       call MPI_BCast(writer_rank,1,MPI_INTEGER,0,arrdes%iogathercomm,status)
       __VERIFY(STATUS)
       arrdes%writer_id = writer_rank

       __RETURN(__SUCCESS)

    end subroutine ArrDescrCreateWriterComm

    subroutine ArrDescrCreateReaderComm(arrdes, full_comm, num_readers, rc)
       type(ArrDescr), intent(inout) :: arrdes
       integer, intent(in) :: full_comm
       integer, intent(in) :: num_readers
       integer, optional, intent(out) :: rc

       integer :: status, nx, ny, color, ny_by_readers, myid, j

       nx = size(arrdes%i1)
       ny = size(arrdes%j1)
       __ASSERT(num_readers <= ny,'num readers must be less than or equal to NY')
       __ASSERT(mod(ny,num_readers)==0,'num readers must evenly divide NY')

       call mpi_comm_rank(full_comm,myid, status)
       __VERIFY(status)
       color =  arrdes%NX0
       call MPI_COMM_SPLIT(full_comm, color, MYID, arrdes%Ycomm, status)
       __VERIFY(status)
       color = arrdes%NY0
       call MPI_COMM_SPLIT(full_comm, color, MYID, arrdes%Xcomm, status)
       __VERIFY(status)
       ny_by_readers = ny/num_readers
       if (mod(myid,nx*ny/num_readers) == 0) then
          color = 0
       else
          color = MPI_UNDEFINED
       endif
       call MPI_COMM_SPLIT(full_comm, color, MYID, arrdes%readers_comm, status)
       __VERIFY(status)
       if (num_readers==ny) then
          call MAPL_Comm_dup(arrdes%Xcomm, ArrDes%IOscattercomm, status)
       else
          j = arrdes%NY0 - mod(arrdes%NY0-1,ny_by_readers)
          call MPI_COMM_SPLIT(full_comm, j, MYID, arrdes%IOscattercomm, status)
          __VERIFY(status)
       endif

       __RETURN(__SUCCESS)

    end subroutine ArrDescrCreateReaderComm

  subroutine ArrayScatterShmR4D1(local_array, global_array, grid, mask, rc)

! Mask is really a permutation on the first dimension

    real,         intent(  OUT) :: local_array(:)
!    TYPE_(kind=EKIND_), target, intent(IN   ) :: global_array DIMENSIONS_
    real, target                :: global_array(:)
    type (ESMF_Grid)                          :: grid
    integer, optional,          intent(IN   ) :: mask(:)
    integer, optional,          intent(  OUT) :: rc

! Local variables

    integer                               :: status

    real,    pointer        :: myglob(:) => null()
    real,    pointer        :: VAR(:)
    type (ESMF_DistGrid)                  :: distGrid
    type(ESMF_DELayout)                   :: LAYOUT
    type (ESMF_VM)                        :: vm
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer, dimension(:), allocatable    :: SENDCOUNTS, DISPLS
    integer                               :: KK
    integer                               :: nDEs
    integer                               :: recvcount
    integer                               :: I, K, II, deId
    integer                               :: gridRank
    integer                               :: LX
    integer                               :: srcPE
    integer                               :: ISZ
    logical                               :: alloc_var
    logical                               :: use_shmem

! Works only on 1D and 2D arrays
! Note: for tile variables the gridRank is 1
! and the case RANK_=2 needs additional attention

! use_shmem controls communication (bcastToNodes+local copy vs scatterv)
    use_shmem = .true.

    ! temporary Shmem restricted only to 1d and tile vars
    if (.not.present(mask)) use_shmem = .false.

! Optional change of source PE. Default=MAPL_Root

    srcPE = MAPL_Root

! Initialize
    alloc_var = .true.

! Get grid and layout information

    call ESMF_GridGet    (GRID,   dimCount=gridRank, rc=STATUS);__VERIFY(STATUS)
    call ESMF_GridGet    (GRID,   distGrid=distGrid, rc=STATUS);__VERIFY(STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS);__VERIFY(STATUS)
    call ESMF_DELayoutGet(layout, vm=vm, rc=status);__VERIFY(STATUS)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, rc=status);__VERIFY(STATUS)

    if (use_shmem) then
       srcPE = deId
    end if

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    __VERIFY(STATUS)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)
    __VERIFY(STATUS)
    allocate (sendcounts(0:nDEs-1), stat=status)
    __VERIFY(STATUS)
    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, rc=status)
    __VERIFY(STATUS)

    ISZ = size(GLOBAL_ARRAY,1)

    if (use_shmem) then
       call MAPL_SyncSharedMemory(rc=STATUS)
       __VERIFY(STATUS)
       call MAPL_BroadcastToNodes(global_array, N=ISZ, ROOT=MAPL_Root, rc=status)
       __VERIFY(STATUS)
       call MAPL_SyncSharedMemory(rc=STATUS)
       __VERIFY(STATUS)
    end if

! Compute count to be sent to each PE

    if(present(mask)) then
       sendcounts = 0
       do II = 1,ISZ
          sendcounts(mask(ii)) = sendcounts(mask(ii)) + 1
       enddo
    else
       do I = 0,nDEs-1
          LX = AU(1,I) - AL(1,I) + 1
          sendcounts(I) = LX
       end do
    end if

! Count I will recieve

    recvcount = sendcounts(deId)

! Put VAR together at the srcPE

    if (deId == srcPE) then

       allocate(DISPLS(0:nDEs          ), stat=status)
       __VERIFY(STATUS)

! Compute displacements into the VAR vector

       displs(0) = 0
       do I = 1,nDEs
          displs(I) = displs(I-1) + sendcounts(I-1)
       end do

       myglob => global_array

! Fill the VAR vector

       if (present(mask)) then
          allocate(VAR(displs(deId):displs(deId+1)-1), stat=status)
          __VERIFY(STATUS)
          KK = DISPLS(deId)

          do I=1,ISZ
             K = MASK(I)
             if(K == deId) then
                II = KK
                VAR(II) = MYGLOB(I)
                KK = KK + 1
             end if
          end do

       else

          var => myglob
          alloc_var = .false.

       endif !  present(mask)

     else
        allocate(var(0:1), stat=status)
        __VERIFY(STATUS)
        allocate(DISPLS(0:nDEs), stat=status)
        __VERIFY(STATUS)
     end if !  I am srcPEa


! Do the communications
    if (use_shmem) then
       ! copy my piece from var (var is local but was filled from shared array)
       call MAPL_SyncSharedMemory(rc=STATUS)
       __VERIFY(STATUS)
       local_array = var(displs(deId):displs(deId+1)-1)
       call MAPL_SyncSharedMemory(rc=STATUS)
       __VERIFY(STATUS)
    else
       call MAPL_CommsScatterV(layout, var, sendcounts, displs, &
                               local_array, recvcount, srcPE, status)
       __VERIFY(STATUS)
    end if

! Clean-up

    deallocate(displs, stat=status)
    __VERIFY(STATUS)
    if(alloc_var) then
       deallocate(VAR, stat=status)
       __VERIFY(STATUS)
    end if

    deallocate(sendcounts, stat=status)
    __VERIFY(STATUS)
    deallocate(AU,         stat=status)
    __VERIFY(STATUS)
    deallocate(AL,         stat=status)
    __VERIFY(STATUS)

! All done

    __RETURN(ESMF_SUCCESS)
  end subroutine ArrayScatterShmR4D1

  subroutine ArrDescrCommFree(arrdes, rc)
    type(ArrDescr), intent(inout) :: arrdes
    integer, optional, intent(out) :: rc

    integer :: status

    call MAPL_CommFree(arrdes%Xcomm, __RC)
    call MAPL_CommFree(arrdes%Ycomm, __RC)
    call MAPL_CommFree(arrdes%readers_comm, __RC)
    call MAPL_CommFree(arrdes%writers_comm, __RC)
    call MAPL_CommFree(arrdes%IOgathercomm, __RC)
    call MAPL_CommFree(arrdes%IOscattercomm, __RC)
    
    __RETURN(ESMF_SUCCESS)
  end subroutine ArrDescrCommFree

  subroutine MAPL_CommFree(comm, rc)
    integer, intent(inout) :: comm
    integer, optional, intent(out) :: rc

    integer :: status

    if(comm /= MPI_COMM_NULL) then
       call MPI_Comm_Free(comm, status)
       __VERIFY(status)
       comm = MPI_COMM_NULL
    end if
    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CommFree

  subroutine MAPL_Comm_Dup(comm, newcomm, rc)
    integer, intent(in) :: comm
    integer, intent(out) :: newcomm
    integer, optional, intent(out) :: rc

    integer :: status

    newcomm = MPI_COMM_NULL 
    if(comm /= MPI_COMM_NULL) then
       call MPI_Comm_Dup(comm, newcomm,status)
       __VERIFY(status)
    end if
    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_Comm_Dup
end module FileIOSharedMod

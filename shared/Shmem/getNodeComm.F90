#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) getNodeComm_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
      use MAPL_SortMod
  implicit none

contains

    module function getNodeComm(Comm, rc) result(NodeComm)
       integer,           intent( IN) :: Comm
       integer, optional, intent(OUT) :: rc
       integer                        :: NodeComm

      integer, allocatable :: colors(:), ranks(:)
      integer :: last
      integer :: i, n

      character(len=MPI_MAX_PROCESSOR_NAME) :: name
      character(len=MPI_MAX_PROCESSOR_NAME), allocatable :: names(:)

      integer :: len, STATUS, MyColor, NumColors, npes, rank
      integer :: NumCores
      integer :: nodeRank

      integer :: i1, i2
      integer, allocatable :: newNode(:)
      class(Logger), pointer :: lgr

      NodeComm=MPI_COMM_NULL

      call MPI_Get_processor_name(name,len,STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      call MPI_COMM_RANK(Comm, rank, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      call MPI_COMM_SIZE(Comm, npes, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      allocate(names(npes),stat=STATUS)
      _ASSERT(STATUS==0,'needs informative message')

      call MPI_AllGather(name ,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,&
                         names,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,Comm,status)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      myColor = getColor(name, names)

      ! We are ready to split communicators

      call MPI_COMM_SPLIT(Comm, MyColor, rank, NodeComm, STATUS)
      _ASSERT(NodeComm/=MPI_COMM_NULL,'needs informative message')

      call MPI_COMM_SIZE(NodeComm, NumCores, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      allocate(colors(npes), ranks(npes), stat=STATUS)
      _ASSERT(STATUS==0,'needs informative message')
      do i=1,npes
         ranks(i) = i-1
      end do

      call MPI_AllGather(myColor, 1, MPI_INTEGER,&
                         colors,  1, MPI_INTEGER,Comm,status)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      call MAPL_Sort(COLORS,ranks)
      last = 0
      n = 0
      allocate(newNode(npes+1),stat=status)
      _VERIFY(STATUS)
      newNode = 0
      do i=1,npes
         if(last /= colors(i)) then
            last = colors(i)
            n = n + 1
            newNode(n) = i
         end if
         ! We are done with COLORS, now we reuse the space to store NodeId
         colors(i) = n
      end do
      NumColors = n
      MAPL_NumNodes = NumColors
      do i=1,size(ranks)
         if (ranks(i) == rank) then
            MAPL_MyNodeNum = colors(i)
            exit
         end if
      end do

      newNode(NumColors+1) = npes+1
      allocate(MAPL_NodeRankList(NumColors), stat=status)
      _VERIFY(STATUS)
      do i=1,NumColors
         i1=newNode(i)
         i2=newNode(i+1)-1
         allocate(MAPL_NodeRankList(i)%rank(i2-i1+1),stat=status)
         _VERIFY(STATUS)
         MAPL_NodeRankList(i)%rank=ranks(i1:i2)
         call MAPL_Sort(MAPL_NodeRankList(i)%rank)
      end do
      deallocate(newNode)

      do i=1,size(MAPL_NodeRankList)
         MAPL_NodeRankList(i)%RankLastUsed=1
      enddo

      deallocate(ranks)
      deallocate(colors)

      MAPL_CoresPerNodeUsed = NumCores

      call MPI_Comm_rank(NodeComm, nodeRank, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      MAPL_AmNodeRoot = nodeRank==0

      if (MAPL_AmNodeRoot) then
         _ASSERT(MAPL_NodeRankList(MAPL_MyNodeNum)%rank(1) == rank,'needs informative message')
      end if

!     we store the global Min and Max of CoresPerNode
      call MPI_AllReduce (MAPL_CoresPerNodeUsed, MAPL_CoresPerNodeMin, &
                          1, MPI_INTEGER, MPI_MIN, comm, status )
      _VERIFY(STATUS)
      call MPI_AllReduce (MAPL_CoresPerNodeUsed, MAPL_CoresPerNodeMax, &
                          1, MPI_INTEGER, MPI_MAX, comm, status )
      _VERIFY(STATUS)

      lgr => logging%get_logger('MAPL.SHMEM')

      if(rank==0) then
         if (MAPL_CoresPerNodeMin == MAPL_CoresPerNodeMax) then
            call lgr%info("NumCores per Node = %i0", NumCores)
         else
            call lgr%info("NumCores per Node varies from %i0 to %i0", &
                 MAPL_CoresPerNodeMin, MAPL_CoresPerNodeMax)
         end if
         call lgr%info("NumNodes in use   = %i0", NumColors)
         call lgr%info("Total PEs         = %i0", npes)
      end if

      deallocate(names,stat=STATUS)
      _ASSERT(STATUS==0,'needs informative message')

      _RETURN(SHM_SUCCESS)
    contains
      function getColor(name, sampleNames) result(color)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: sampleNames(:)
        integer :: color

        integer :: i

        color = 0 ! unless
        do i = 1, size(sampleNames)
           if (trim(name) == trim(sampleNames(i))) then
              color = i
              exit
           end if
        end do

      end function getColor

    end function getNodeComm

end submodule getNodeComm_smod


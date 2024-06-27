#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) getNodeRootsComm_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module function getNodeRootsComm(Comm, rc) result(NodeRootsComm)
       integer,           intent( IN) :: Comm
       integer, optional, intent(OUT) :: rc
       integer                        :: NodeRootsComm

      integer :: STATUS, MyColor, NumNodes, npes, rank
      class(Logger), pointer :: lgr

      NodeRootsComm=MPI_COMM_NULL

      call MPI_COMM_RANK(Comm, rank, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
      call MPI_COMM_SIZE(Comm, npes, STATUS)
      _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')

      myColor = 0
      if (MAPL_AmNodeRoot) myColor = 1

      ! We are ready to split communicators

      call MPI_COMM_SPLIT(Comm, MyColor, rank, NodeRootsComm, STATUS)
      _ASSERT(NodeRootsComm/=MPI_COMM_NULL,'needs informative message')

      if (myColor==0) then
      ! Set nodes outside of this comm back to null
         NodeRootsComm=MPI_COMM_NULL
      else
      ! Confirm we have the proper communicator
         call MPI_COMM_SIZE(NodeRootsComm, NumNodes, STATUS)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
         ! additional sanity checks
         _ASSERT(MAPL_NumNodes == NumNodes,'needs informative message')
         call MPI_COMM_RANK(NodeRootsComm, rank, STATUS)
         _ASSERT(STATUS==MPI_SUCCESS,'needs informative message')
         _ASSERT(MAPL_MyNodeNum == rank+1,'needs informative message')
      endif

      lgr => logging%get_logger('MAPL.SHMEM')

      if(rank==0) then
         call lgr%info("NumNodes in use  = %i0", NumNodes)
      end if

      _RETURN(SHM_SUCCESS)

    end function getNodeRootsComm

end submodule getNodeRootsComm_smod


#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_GetNewRank_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module function MAPL_GetNewRank(node,rc) result(rank)
       integer :: rank
       integer, intent(in) :: node
       integer, optional, intent(out) :: rc

      rank = MAPL_NodeRankList(node)%RankLastUsed+1
      if (rank > size(MAPL_NodeRankList(node)%rank)) then
         rank = 1
      end if
      MAPL_NodeRankList(node)%rankLastUsed=rank

      _RETURN(SHM_SUCCESS)

    end function MAPL_GetNewRank
end submodule MAPL_GetNewRank_smod


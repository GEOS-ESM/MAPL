#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
module MAPL_CommGroupDescriptionMod
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   implicit none
   private

   public :: CommGroupDescription
   
   type :: CommGroupDescription
      integer :: npes_per_node
      integer :: npes
      integer :: nnodes
      logical :: isolate_nodes
      character(:), allocatable :: name
   contains
      procedure :: comm_group_range
   end type CommGroupDescription

interface CommGroupDescription
   module procedure new_CommGroupDescription
end interface

contains

   function new_CommGroupDescription( npes, nnodes, isolate_nodes, name, unusable, npes_per_node, rc) result(CommGroup)
      type(CommGroupDescription) :: CommGroup
      integer, intent(in) :: npes
      integer, intent(in) :: nnodes
      logical, intent(in) :: isolate_nodes
      character(*), intent(in) :: name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in)  :: npes_per_node
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _ASSERT(npes*nnodes == 0, "npes and nnodes are exclusive")

      if (nnodes > 0) then
         _ASSERT( isolate_nodes, " nnodes should be isolated")
      endif

      CommGroup%npes   = npes
      CommGroup%nnodes = nnodes
      CommGroup%isolate_nodes = isolate_nodes 
      CommGroup%name = name
      CommGroup%npes_per_node = 0

      if ( present(npes_per_node)) then
         CommGroup%npes_per_node = npes_per_node
         CommGroup%npes   = 0
         CommGroup%nnodes = 0
      endif

      _RETURN(_SUCCESS)
   end function new_CommGroupDescription
 
   subroutine comm_group_range(this,my_node, my_rank, nodes_sizes, start_node, start_rank, &
                               next_node, next_rank, IamInGroup, unusable, rc)
      class(CommGroupDescription), intent(in) :: this
      integer, intent(in)   :: my_node, my_rank
      integer, intent(in)   :: nodes_sizes(:)
      integer, intent(in)   :: start_node, start_rank
      integer, intent(out)  :: next_node, next_rank
      logical, intent(inout)  :: IamInGroup
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: np, start_node_, start_rank_, i_node, i_rank

       _UNUSED_DUMMY(unusable)
      
       start_node_ = start_node
       start_rank_ = start_rank

       if (this%isolate_nodes .and. start_rank /= 0) then
          start_node_ = start_node_ + 1
          start_rank_ = 0
       endif

       ! case 1, group is divided by npes
       if(this%npes > 0) then
         np = 0
         outer: do i_node = start_node_, size(nodes_sizes)
           do i_rank = start_rank_, nodes_sizes(i_node)-1

              if( my_node == i_node .and. my_rank == i_rank) then
                  _ASSERT( .not. IamInGroup, "I have been included in the other group")
                  IamInGroup = .true.
              endif

              np = np+1
              if (np == this%npes) then
                 next_node = i_node
                 next_rank = i_rank + 1  
                 if (this%isolate_nodes .or. next_rank > nodes_sizes(i_node)-1 ) then
                    next_rank = 0
                    next_node = i_node + 1
                 endif
                 exit outer
              endif
           enddo
        enddo outer
      endif

      ! case 2, group is divided by nnodes
      if (this%nnodes > 0) then

         next_node = start_node_ + this%nnodes 
         next_rank = 0
         if (start_node_ <= my_node .and. my_node < next_node) then
            _ASSERT( .not. IamInGroup, "I have been included in the other group")
            IamInGroup = .true.
         endif
      endif

      ! case 3, npes per node
      if (this%npes_per_node > 0) then
         _ASSERT( all(nodes_sizes == nodes_sizes(1)) , " all nodes should have the same amount of cores")
         next_rank  = start_rank + this%npes_per_node
         next_node  = 0 ! no used
         if (start_rank <=my_rank .and. my_rank < next_rank) then
            _ASSERT( .not. IamInGroup, "I have been included in the other group")
            IamInGroup = .true.
         endif
      endif

      ! case 4, empty group
      if (this%npes == 0 .and. this%nnodes ==0 .and. this%npes_per_node ==0) then
        next_node = start_node
        next_rank = start_rank
      endif

      _RETURN(_SUCCESS)
   end subroutine comm_group_range
 
end module MAPL_CommGroupDescriptionMod

module MAPL_CommGroupDescriptionVectorMod
   use MAPL_CommGroupDescriptionMod

#define _type type(CommGroupDescription)
#define _vector CommGroupDescriptionVector
#define _iterator CommGroupDescriptionVectorIterator

#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _type
end module MAPL_CommGroupDescriptionVectorMod


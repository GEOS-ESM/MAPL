#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_AbstractCommSplitterMod
   use MPI
   implicit none
   private

   public :: AbstractCommSplitter

   type, abstract :: AbstractCommSplitter
      private
      integer :: shared_communicator
   contains
      procedure(split), deferred :: split
      procedure :: set_shared_communicator
      procedure :: get_shared_communicator
   end type AbstractCommSplitter


   abstract interface

      function split(this, unusable, rc) result(split_communicator)
         use MAPL_KeywordEnforcerMod
         use MAPL_SplitCommunicatorMod
         import AbstractCommSplitter
         type (SplitCommunicator) :: split_communicator
         class (AbstractCommSplitter), intent(in) :: this
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end function split

   end interface

contains

   
   integer function get_shared_communicator(this) result(shared_communicator)
      class (AbstractCommSplitter), intent(in) :: this

      shared_communicator = this%shared_communicator

   end function get_shared_communicator


   subroutine set_shared_communicator(this, shared_communicator)
      class (AbstractCommSplitter), intent(inout) :: this
      integer, intent(in) :: shared_communicator

      this%shared_communicator = shared_communicator

   end subroutine set_shared_communicator

end module MAPL_AbstractCommSplitterMod

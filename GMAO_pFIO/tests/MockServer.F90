#include "unused_dummy.H"
module MockServerMod

   use pFIO_BaseServerMod
   use pFIO_AbstractDataReferenceMod

   implicit none
   private

   public :: MockServer

   type,extends (BaseServer) :: MockServer
      character(len=16) :: port_name
   contains
      procedure :: start
   end type MockServer


   interface MockServer
      module procedure new_MockServer
   end interface MockServer


contains

   function new_MockServer(comm) result(s)
      type (MockServer) :: s
      integer, optional, intent(in) :: comm

      if (present(comm)) s%comm = comm
      allocate(s%serverthread_done_msgs(1))
      s%serverthread_done_msgs(:) = .true.

   end function new_MockServer

   subroutine start(this)
      class (MockServer),target, intent(inout) :: this
   end subroutine

end module MockServerMod

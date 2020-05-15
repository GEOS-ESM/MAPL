#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
module MockServerThreadMod
   use MAPL_ExceptionHandling
   use pFIO_ServerThreadMod
   use pFIO_AbstractMessageMod
   use pFIO_MessageVisitorMod
   use pFIO_AbstractSocketMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_PrefetchDoneMessageMod
   use pFIO_AddExtCollectionMessageMod
   use pFIO_IdMessageMod
   use pFIO_PrefetchDataMessageMod

   implicit none
   private

   public :: MockServerThread

   type, extends(ServerThread) :: MockServerThread
      character(len=:), allocatable :: log
   contains
      procedure :: prefix
      procedure :: handle_Terminate
      procedure :: handle_Done
      procedure :: handle_Done_prefetch
      procedure :: handle_AddExtCollection
      procedure :: handle_PrefetchData
   end type MockServerThread

   interface MockServerThread
      module procedure new_MockServerThread
   end interface MockServerThread

contains

   function new_MockServerThread(sckt) result(s)
      type (MockServerThread) :: s
      class (AbstractSocket), target, intent(in) :: sckt
      call s%set_connection(sckt)
   end function new_MockServerThread

   subroutine prefix(this, string)
      class (MockServerThread), intent(inout) :: this
      character(len=*), intent(in) :: string

      if (allocated(this%log)) then
         this%log = this%log // ' :: ' // string
      else
         this%log = string
      end if

   end subroutine prefix

   subroutine handle_Terminate(this, message, rc)
      class (MockServerThread), intent(inout) :: this
      type (TerminateMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(message)
      call this%prefix('handle_Terminate()')
      call this%set_terminate()
      _RETURN(_SUCCESS)
   end subroutine handle_Terminate

   subroutine handle_Done(this, message, rc)
      class (MockServerThread), target, intent(inout) :: this
      type (DoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(message)
      call this%prefix('handle_Done()')
      _RETURN(_SUCCESS)
   end subroutine handle_Done

   subroutine handle_Done_prefetch(this, message, rc)
      class (MockServerThread), target, intent(inout) :: this
      type (PrefetchDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(message)
      call this%prefix('handle_Done_prefetch()')
      _RETURN(_SUCCESS)
   end subroutine handle_Done_prefetch

   subroutine handle_AddExtCollection(this, message, rc)
      class (MockServerThread), target, intent(inout) :: this
      type (AddExtCollectionMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(message)

      call this%prefix('handle_AddExtCollection()')
      _RETURN(_SUCCESS)
   end subroutine handle_AddExtCollection

   subroutine handle_PrefetchData(this, message, rc)
      class (MockServerThread), intent(inout) :: this
      type (PrefetchDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(message)
      call this%prefix('handle_PrefetchData()')
      _RETURN(_SUCCESS)
   end subroutine handle_PrefetchData

end module MockServerThreadMod

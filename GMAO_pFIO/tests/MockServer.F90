#include "unused_dummy.H"
module MockServerThreadMod
   use pFIO_ServerThreadMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractSocketMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_AddCollectionMessageMod
   use pFIO_CollectionIdMessageMod
   use pFIO_RequestIdMessageMod
   use pFIO_RequestDataMessageMod
   use pFIO_WaitRequestDataMessageMod

   implicit none
   private

   public :: MockServer

   type, extends(Server) :: MockServer
      character(len=:), allocatable :: log
   contains
      procedure :: prefix
      procedure :: handle_Terminate
      procedure :: handle_Done
      procedure :: handle_AddCollection
      procedure :: handle_RequestData
   end type MockServer

   interface MockServer
      module procedure new_MockServer
   end interface MockServer

contains

   function new_MockServer() result(s)
      type (MockServer) :: s
      _UNUSED_DUMMY(s)
   end function new_MockServer

   subroutine prefix(this, string)
      class (MockServer), intent(inout) :: this
      character(len=*), intent(in) :: string

      if (allocated(this%log)) then
         this%log = this%log // ' :: ' // string
      else
         this%log = string
      end if

   end subroutine prefix


   subroutine handle_Terminate(this, message)
      class (MockServer), intent(inout) :: this
      type (TerminateMessage), intent(in) :: message

      _UNUSED_DUMMY(message)
      call this%prefix('handle_Terminate()')
      call this%set_terminate()

   end subroutine handle_Terminate

   subroutine handle_Done(this, message)
      class (MockServer), intent(inout) :: this
      type (DoneMessage), intent(in) :: message

      _UNUSED_DUMMY(message)
      call this%prefix('handle_Done()')

   end subroutine handle_Done

   subroutine handle_AddCollection(this, message)
      class (MockServer), intent(inout) :: this
      type (AddCollectionMessage), intent(in) :: message

      _UNUSED_DUMMY(message)

      call this%prefix('handle_AddCollection()')

   end subroutine handle_AddCollection

   subroutine handle_RequestData(this, message)
      class (MockServer), intent(inout) :: this
      type (RequestDataMessage), intent(in) :: message

      _UNUSED_DUMMY(message)
      call this%prefix('handle_RequestData()')

   end subroutine handle_RequestData

end module MockServerMod

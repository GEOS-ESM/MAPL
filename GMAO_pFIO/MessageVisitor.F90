#define _ACTION_OVERLOAD_METHOD 10
#define _ACTION_ABSTRACT_INTERFACE 11
#define _HANDLE_ID 1

module pFIO_MessageVisitorMod
   use pFIO_AbstractMessageMod
   use pFIO_DoneMessageMod
   use pFIO_AddCollectionMessageMod
   use pFIO_CollectionIdMessageMod
   use pFIO_RequestIdMessageMod
   use pFIO_RequestDataMessageMod
   use pFIO_CollectiveRequestDataMessageMod
   use pFIO_WaitRequestDataMessageMod
   use pFIO_TerminateMessageMod
   use pFIO_DummyMessageMod
   use pFIO_AbstractRequestHandleMod 
   implicit none
   private

   public :: MessageVisitor

   type, abstract, extends(SurrogateMessageVisitor) :: MessageVisitor
   contains
      procedure, non_overridable :: handle

      procedure :: handle_Done
      procedure :: handle_AddCollection
      procedure :: handle_CollectionId
      procedure :: handle_RequestId              
      procedure :: handle_RequestData
      procedure :: handle_CollectiveRequestData
      procedure :: handle_WaitRequestData   
      procedure :: handle_Terminate
      procedure :: handle_Dummy
      
      generic :: handle_cmd => handle_Done
      generic :: handle_cmd => handle_AddCollection
      generic :: handle_cmd => handle_CollectionId
      generic :: handle_cmd => handle_RequestId
      generic :: handle_cmd => handle_RequestData
      generic :: handle_cmd => handle_CollectiveRequestData
      generic :: handle_cmd => handle_WaitRequestData
      generic :: handle_cmd => handle_Terminate
      generic :: handle_cmd => handle_Dummy

      procedure :: print
   end type MessageVisitor

contains

   subroutine print(this)
      class(MessageVisitor), intent(in) :: this
      print*,'Visitor type: abstract'
   end subroutine print

   subroutine handle(this, message)
      class (MessageVisitor), intent(inout) :: this
      class (AbstractMessage), intent(in) :: message

      select type (cmd => message)
      type is (TerminateMessage)
        call this%handle_terminate(cmd)
      type is (DoneMessage)
        call this%handle_cmd(cmd)
      type is (AddCollectionMessage)
        call this%handle_AddCollection(cmd)
      type is (CollectionIdMessage)
        call this%handle_cmd(cmd)
      type is (RequestIdMessage)
        call this%handle_cmd(cmd)
      type is (RequestDataMessage)
        call this%handle_cmd(cmd)
      type is (CollectiveRequestDataMessage)
        call this%handle_cmd(cmd)
      type is (WaitRequestDataMessage)
        call this%handle_cmd(cmd)
      type is (DummyMessage)
        call this%handle_cmd(cmd)
      class default
         stop 'unsupported subclass'
      end select

   end subroutine handle

   subroutine handle_CollectiveRequestData(this, message)
      class (MessageVisitor), intent(inout) :: this
      type (CollectiveRequestDataMessage), intent(in) :: message
      print*, "Warning : dummy handle_CollectiveRequestData should not be called"
   end subroutine handle_CollectiveRequestData

   subroutine handle_Terminate(this, message)
      class (MessageVisitor), intent(inout) :: this
      type (TerminateMessage), intent(in) :: message
      print*, "Warning : dummy handle_Terminate should not be called"
   end subroutine handle_Terminate

   subroutine handle_Done(this, message)
      class (MessageVisitor), target, intent(inout) :: this
      type (DoneMessage), intent(in) :: message
      print*, "Warning : dummy handle_Done should not be called"
   end subroutine handle_Done

   subroutine handle_AddCollection(this, message)
      class (MessageVisitor), target, intent(inout) :: this
      type (AddCollectionMessage), intent(in) :: message
      print*, "Warning : dummy handle_AddCollection should not be called"
   end subroutine handle_AddCollection

   subroutine handle_CollectionId(this, message)
      class (MessageVisitor), intent(inout) :: this
      type (CollectionIdMessage), intent(in) :: message
      print*, "Warning : dummy handle_CollectionID should not be called"
   end subroutine handle_CollectionId

   subroutine handle_RequestId(this, message)
      class (MessageVisitor), intent(inout) :: this
      type (RequestIdMessage), intent(in) :: message
      print*, "Warning : dummy handle_RequestID is called"
   end subroutine handle_RequestId

   subroutine handle_RequestData(this, message)
      class (MessageVisitor), intent(inout) :: this
      type (RequestDataMessage), intent(in) :: message
      print*, "Warning : dummy handle_RequestData should not be called"
   end subroutine handle_RequestData

   subroutine handle_WaitRequestData(this, message)
      class (MessageVisitor), target, intent(inout) :: this
      type (WaitRequestDataMessage), intent(in) :: message
      print*, "Warning : dummy handle_WaitRequestData should not be called"
   end subroutine handle_WaitRequestData

   subroutine handle_Dummy(this, message)
      class (MessageVisitor), target, intent(inout) :: this
      type (DummyMessage), intent(in) :: message
   end subroutine handle_Dummy
end module pFIO_MessageVisitorMod

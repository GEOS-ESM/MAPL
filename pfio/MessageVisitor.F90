#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MessageVisitorMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_DoneMessageMod
   use pFIO_PrefetchDoneMessageMod
   use pFIO_CollectivePrefetchDoneMessageMod
   use pFIO_StageDoneMessageMod
   use pFIO_CollectiveStageDoneMessageMod
   use pFIO_AddExtCollectionMessageMod
   use pFIO_AddHistCollectionMessageMod
   use pFIO_IdMessageMod
   use pFIO_PrefetchDataMessageMod
   use pFIO_CollectivePrefetchDataMessageMod
   use pFIO_StageDataMessageMod
   use pFIO_CollectiveStageDataMessageMod
   use pFIO_TerminateMessageMod
   use pFIO_DummyMessageMod
   use pFIO_HandShakeMessageMod
   use pFIO_ModifyMetadataMessageMod
   use pFIO_AbstractRequestHandleMod 
   implicit none
   private

   public :: MessageVisitor

   type, abstract, extends(SurrogateMessageVisitor) :: MessageVisitor
   contains
      procedure :: handle

      procedure :: handle_Done
      procedure :: handle_Done_prefetch
      procedure :: handle_Done_collective_prefetch
      procedure :: handle_Done_stage
      procedure :: handle_Done_collective_stage

      procedure :: handle_AddExtCollection
      procedure :: handle_AddHistCollection
      procedure :: handle_Id
      procedure :: handle_PrefetchData
      procedure :: handle_StageData
      procedure :: handle_CollectivePrefetchData
      procedure :: handle_CollectiveStageData
      procedure :: handle_Terminate
      procedure :: handle_ModifyMetadata
      procedure :: handle_HandShake
      
      generic :: handle_cmd => handle_Done
      generic :: handle_cmd => handle_Done_prefetch
      generic :: handle_cmd => handle_Done_collective_prefetch
      generic :: handle_cmd => handle_Done_stage
      generic :: handle_cmd => handle_Done_collective_stage
      generic :: handle_cmd => handle_AddExtCollection
      generic :: handle_cmd => handle_AddHistCollection
      generic :: handle_cmd => handle_Id
      generic :: handle_cmd => handle_PrefetchData
      generic :: handle_cmd => handle_CollectivePrefetchData
      generic :: handle_cmd => handle_StageData
      generic :: handle_cmd => handle_CollectiveStageData
      generic :: handle_cmd => handle_Terminate
      generic :: handle_cmd => handle_ModifyMetadata
      generic :: handle_cmd => handle_HandShake

   end type MessageVisitor

contains

   recursive subroutine handle(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      class (AbstractMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      integer :: status

      select type (cmd => message)
      type is (TerminateMessage)
        call this%handle_terminate(cmd, rc=status)
        _VERIFY(status)
      type is (DoneMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (PrefetchDoneMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (CollectivePrefetchDoneMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (StageDoneMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (CollectiveStageDoneMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (AddExtCollectionMessage)
        call this%handle_AddExtCollection(cmd,rc=status)
        _VERIFY(status)
      type is (AddHistCollectionMessage)
        call this%handle_AddHistCollection(cmd,rc=status)
        _VERIFY(status)
      type is (IdMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (PrefetchDataMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (CollectivePrefetchDataMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (StageDataMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (CollectiveStageDataMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (ModifyMetadataMessage)
        call this%handle_cmd(cmd,rc=status)
        _VERIFY(status)
      type is (HandShakeMessage)
        ! the handShake is from client to server
        call this%handle_cmd(cmd, rc=status)
        _VERIFY(status)
      type is (DummyMessage)
        ! WY notes: self hand_shake: if iserver or oserver is with app"
        ! the dummy is from server to client
        ! if the serverthread sends the dummy directly to clientthread, it will not go through here. 
        _VERIFY(0)
      class default
         _ASSERT(.false., 'unsupported subclass')
      end select
      _RETURN(_SUCCESS)
   end subroutine handle

   subroutine handle_CollectivePrefetchData(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      type (CollectivePrefetchDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_CollectivePrefetchData should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_CollectivePrefetchData

   subroutine handle_CollectiveStageData(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      type (CollectiveStageDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_CollectiveStageData should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_CollectiveStageData

   subroutine handle_Terminate(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      type (TerminateMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_Terminate should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_Terminate

   subroutine handle_Done(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (DoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_Done should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_Done

   subroutine handle_Done_prefetch(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (PrefetchDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_Done_prefetch should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_Done_prefetch

   subroutine handle_Done_collective_prefetch(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (CollectivePrefetchDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_Done_collective_prefetch should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_Done_collective_prefetch

   subroutine handle_Done_stage(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (StageDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_Done_stage should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_Done_stage

   subroutine handle_Done_collective_stage(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (CollectiveStageDoneMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_Done_collective_stage should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_Done_collective_stage

   subroutine handle_AddExtCollection(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (AddExtCollectionMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_AddExtCollection should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_AddExtCollection

   subroutine handle_AddHistCollection(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (AddHistCollectionMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_AddHistCollection should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_AddHistCollection

   subroutine handle_Id(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      type (IdMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_ID should not be called. hint: maybe server and app are the same?")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_Id

   subroutine handle_PrefetchData(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      type (PrefetchDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_PrefetchData should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_PrefetchData

   subroutine handle_StageData(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      type (StageDataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_StageData should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_StageData

   subroutine handle_ModifyMetadata(this, message, rc)
      class (MessageVisitor), intent(inout) :: this
      type (ModifyMetadataMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_ModifyMetadata should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_ModifyMetadata

   subroutine handle_HandShake(this, message, rc)
      class (MessageVisitor), target, intent(inout) :: this
      type (HandShakeMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      _ASSERT(.false., "Warning : dummy handle_HandShake should not be called")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(message)
   end subroutine handle_HandShake

end module pFIO_MessageVisitorMod

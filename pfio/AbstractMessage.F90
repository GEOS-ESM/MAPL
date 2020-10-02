#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AbstractMessageMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: AbstractMessage
   public :: SurrogateMessageVisitor

   public :: TERMINATE_ID
   public :: DONE_ID
   public :: PrefetchDone_ID
   public :: CollectivePrefetchDone_ID
   public :: StageDone_ID
   public :: CollectiveStageDone_ID
   public :: ADDEXTCOLLECTION_ID
   public :: ADDHISTCOLLECTION_ID
   public :: ID_ID
   public :: PrefetchData_ID
   public :: StageData_ID
   public :: COLLECTIVEPrefetchData_ID
   public :: COLLECTIVEStageData_ID
   public :: ModifyMetadata_ID
   public :: HandShake_ID
   public :: DUMMY_ID
   public :: ForwardData_ID

   enum, bind(c)
      enumerator :: TERMINATE_ID = 1
      enumerator :: DONE_ID
      enumerator :: PrefetchDone_ID
      enumerator :: CollectivePrefetchDone_ID
      enumerator :: StageDone_ID
      enumerator :: CollectiveStageDone_ID
      enumerator :: ADDEXTCOLLECTION_ID
      enumerator :: ADDHISTCOLLECTION_ID
      enumerator :: ID_ID
      enumerator :: PrefetchData_ID
      enumerator :: COLLECTIVEPrefetchData_ID
      enumerator :: StageData_ID
      enumerator :: COLLECTIVEStageData_ID
      enumerator :: ModifyMetadata_ID
      enumerator :: HandShake_ID
      enumerator :: DUMMY_ID
      enumerator :: ForwardData_ID 
   end enum

   type, abstract :: AbstractMessage
   contains
      procedure (get_type_id), deferred, nopass :: get_type_id
      procedure (get_length),  deferred :: get_length
      procedure (serialize),   deferred :: serialize
      procedure (deserialize), deferred :: deserialize
      procedure :: dispatch

   end type AbstractMessage

   type, abstract :: SurrogateMessageVisitor
   contains
     procedure(handle), deferred :: handle
   end type SurrogateMessageVisitor

   abstract interface

     subroutine handle(this, Message, rc)
       import SurrogateMessageVisitor
       import AbstractMessage
       implicit none
       class (SurrogateMessageVisitor), intent(inout) :: this
       class (AbstractMessage), intent(in) :: message
       integer, optional, intent(out) :: rc
     end subroutine handle

     integer function get_type_id() result(type_id)
        implicit none
     end function get_type_id
      
     integer function get_length(this) result(length)
        import AbstractMessage
        implicit none
        class (AbstractMessage), intent(in) :: this
     end function get_length
      
     subroutine serialize(this, buffer, rc)
        import AbstractMessage
        implicit none
        class (AbstractMessage), intent(in) :: this
        integer, optional, intent(out) :: rc
        integer, intent(inout) :: buffer(:)
     end subroutine serialize
      
     subroutine deserialize(this, buffer, rc)
        import AbstractMessage
        implicit none
        class (AbstractMessage), intent(inout) :: this
        integer, intent(in) :: buffer(:)
        integer, optional, intent(out) :: rc
     end subroutine deserialize
      
   end interface

 contains

    recursive subroutine dispatch(this, visitor, rc)
       class (AbstractMessage), intent(in) :: this
       class (SurrogateMessageVisitor), intent(inout) :: visitor
       integer, optional, intent(out) :: rc
       integer :: status

       call visitor%handle(this, rc=status)
       _VERIFY(status)
       _RETURN(_SUCCESS)
    end subroutine dispatch

end module pFIO_AbstractMessageMod

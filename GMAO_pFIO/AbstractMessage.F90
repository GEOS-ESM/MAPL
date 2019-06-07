module pFIO_AbstractMessageMod
   implicit none
   private

   public :: AbstractMessage
   public :: SurrogateMessageVisitor

   public :: TERMINATE_ID
   public :: DONE_ID
   public :: ADDCOLLECTION_ID
   public :: COLLECTIONID_ID
   public :: REQUESTID_ID
   public :: RequestData_ID
   public :: CollectiveRequestData_ID
   public :: WaitRequestData_ID
   public :: DUMMY_ID

   enum, bind(c)
      enumerator :: TERMINATE_ID = 1
      enumerator :: DONE_ID
      enumerator :: ADDCOLLECTION_ID
      enumerator :: COLLECTIONID_ID
      enumerator :: REQUESTID_ID
      enumerator :: RequestData_ID
      enumerator :: WaitRequestData_ID
      enumerator :: CollectiveRequestData_ID
      enumerator :: DUMMY_ID
   end enum

   type, abstract :: AbstractMessage
      private
      integer :: sender_id
      integer :: receiver_id
   contains
      procedure (get_type_id), deferred, nopass :: get_type_id
      procedure (get_length), deferred :: get_length
      procedure (serialize), deferred :: serialize
      procedure (deserialize), deferred :: deserialize
      procedure :: dispatch

      procedure :: set_sender
      procedure :: get_sender

   end type AbstractMessage

   type, abstract :: SurrogateMessageVisitor
   contains
     procedure(print), deferred :: print
     procedure(handle), deferred :: handle
   end type SurrogateMessageVisitor

   abstract interface

     subroutine print(this)
       import SurrogateMessageVisitor
       implicit none
       class(SurrogateMessageVisitor), intent(in) :: this
     end subroutine print

      subroutine handle(this, Message)
        import SurrogateMessageVisitor
        import AbstractMessage
        implicit none
        class (SurrogateMessageVisitor), intent(inout) :: this
        class (AbstractMessage), intent(in) :: message
      end subroutine handle

      integer function get_type_id() result(type_id)
         implicit none
      end function get_type_id
      
      integer function get_length(this) result(length)
         import AbstractMessage
         implicit none
         class (AbstractMessage), intent(in) :: this
      end function get_length
      
      subroutine serialize(this, buffer)
         import AbstractMessage
         implicit none
         class (AbstractMessage), intent(in) :: this
         integer, intent(inout) :: buffer(:)
      end subroutine serialize
      
      subroutine deserialize(this, buffer)
         import AbstractMessage
         implicit none
         class (AbstractMessage), intent(inout) :: this
         integer, intent(in) :: buffer(:)
      end subroutine deserialize
      
   end interface

 contains

   subroutine dispatch(this, visitor)
      class (AbstractMessage), intent(in) :: this
      class (SurrogateMessageVisitor), intent(inout) :: visitor
      
      call visitor%handle(this)

   end subroutine dispatch


   subroutine set_sender(this, sender_id)
      class (AbstractMessage), intent(inout) :: this
      integer, intent(in) :: sender_id

      this%sender_id = sender_id
   end subroutine set_sender


   integer function get_sender(this) result(sender_id)
      class (AbstractMessage), intent(in) :: this
      sender_id = this%sender_id
   end function get_sender
   
end module pFIO_AbstractMessageMod

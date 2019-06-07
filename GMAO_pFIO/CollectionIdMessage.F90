#include "unused_dummy.H"
module pFIO_CollectionIdMessageMod
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: CollectionIdMessage

   type, extends(AbstractMessage) :: CollectionIdMessage
      integer :: collection_id
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type CollectionIdMessage

   interface CollectionIdMessage
      module procedure new_CollectionIdMessage
   end interface CollectionIdMessage

contains

   function new_CollectionIdMessage(collection_id) result(message)
      type (CollectionIdMessage) :: message
      integer, intent(in) :: collection_id

      message%collection_id = collection_id

   end function new_CollectionIdMessage
      


   integer function get_type_id() result(type_id)
      type_id = COLLECTIONID_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (CollectionIdMessage), intent(in) :: this
      _UNUSED_DUMMY(this)
      length = 1
   end function get_length

   subroutine serialize(this, buffer)
      class (CollectionIdMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) ! no-op
      buffer = [this%collection_id]
   end subroutine serialize

   subroutine deserialize(this, buffer)
      class (CollectionIdMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)

      this%collection_id = buffer(1)

   end subroutine deserialize
   
end module pFIO_CollectionIdMessageMod


#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AddHistCollectionMessageMod
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod
   use pFIO_AbstractMessageMod
   use pFIO_FileMetadataMod
   implicit none
   private

   public :: AddHistCollectionMessage

   type, extends(AbstractMessage) :: AddHistCollectionMessage
      type(FileMetadata) :: fmd
      ! WY node: -1    : add ( default )
      !          other : replace
      integer :: collection_id = -1
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type AddHistCollectionMessage

   interface AddHistCollectionMessage
      module procedure new_AddHistCollectionMessage
   end interface AddHistCollectionMessage

contains

   function new_AddHistCollectionMessage(fmd, collection_id) result(message)
      type (AddHistCollectionMessage) :: message
      type(FileMetadata), intent(in) :: fmd
      integer, optional, intent(in) :: collection_id
      message%fmd = fmd
      if( present(collection_id)) message%collection_id = collection_id
   end function new_AddHistCollectionMessage

   
   integer function get_type_id() result(type_id)
      type_id = ADDHISTCOLLECTION_ID
   end function get_type_id


   integer function get_length(this) result(length)
      class (AddHistCollectionMessage), intent(in) :: this
      integer,allocatable :: buffer(:) ! no-op
      call this%fmd%serialize(buffer)
      length = size(buffer) + 1 ! 1 is the collection_id
   end function get_length


   subroutine serialize(this, buffer, rc)
      class (AddHistCollectionMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:) ! no-op
      integer, optional, intent(out) :: rc

      integer,allocatable :: tmp_buffer(:) ! no-op
      integer :: status
      call this%fmd%serialize(tmp_buffer, status)
      _VERIFY(status)
      buffer = [tmp_buffer,serialize_intrinsic(this%collection_id)]
      _RETURN(_SUCCESS)
   end subroutine serialize


   subroutine deserialize(this, buffer,rc)
      class (AddHistCollectionMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer :: n, length, status

      n = 1
      call FileMetaData_deserialize(buffer(n:), this%fmd, status)
      _VERIFY(status)
      call deserialize_intrinsic(buffer(n:), length)
      n = n + length 
      call deserialize_intrinsic(buffer(n:), this%collection_id)
      _RETURN(_SUCCESS)
   end subroutine deserialize

end module pFIO_AddHistCollectionMessageMod

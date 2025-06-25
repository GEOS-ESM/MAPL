#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ReplaceMetadataMessageMod
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod
   use pFIO_FileMetadataMod
   use pFIO_AbstractMessageMod
   use pFIO_StringVariableMapMod
   use pFIO_StringVariableMapUtilMod
   use mapl_KeywordEnforcerMod
   implicit none
   private

   public :: ReplaceMetadataMessage

   type, extends(AbstractMessage) :: ReplaceMetadataMessage
      integer :: collection_id
      type (FileMetadata) :: fmd
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type ReplaceMetadataMessage

   interface ReplaceMetadataMessage
      module procedure new_ReplaceMetadataMessage
   end interface

contains

   function new_ReplaceMetadataMessage(collection_id, fmd, rc) result (message)
      type (ReplaceMetadataMessage) :: message
      integer, intent(in) :: collection_id
      type (FileMetadata), intent(in) :: fmd
      integer, optional, intent(out) :: rc

      message%collection_id = collection_id
      message%fmd = fmd
 
      _return(_success)
   end function new_ReplaceMetadataMessage

   integer function get_type_id() result(type_id)
      type_id = ReplaceMetadata_ID
   end function get_type_id
 
   integer function get_length(this) result(length)
      class (ReplaceMetadataMessage), intent(in) :: this
      integer, allocatable :: buffer(:)

      allocate(buffer(0))

      call this%fmd%serialize(buffer)

      length = &
           & serialize_buffer_length(this%collection_id) + &
           & size(buffer)

   end function get_length

   subroutine serialize(this, buffer, rc)
      class (ReplaceMetadataMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc
  
      integer, allocatable ::  fmd_buf(:)
      integer :: status

      call this%fmd%serialize(fmd_buf, rc=status)
      _verify(status)
      buffer = [ &
           & serialize_intrinsic(this%collection_id), &
           & fmd_buf]
      _return(_success)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (ReplaceMetadataMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer :: n, status

      n = 1
      call deserialize_intrinsic(buffer(n:), this%collection_id)
      n = n + serialize_buffer_length(this%collection_id)

      call FileMetadata_deserialize(buffer(n:), this%fmd, rc=status)
      _verify(status)
      _return(_success)
   end subroutine deserialize

end module pFIO_ReplaceMetadataMessageMod

#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AddExtCollectionMessageMod
   use pFIO_ErrorHandlingMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractMessageMod
   implicit none
   private

   public :: AddExtCollectionMessage

   type, extends(AbstractMessage) :: AddExtCollectionMessage
      character(len=:), allocatable :: template
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type AddExtCollectionMessage

   interface AddExtCollectionMessage
      module procedure new_AddExtCollectionMessage
   end interface AddExtCollectionMessage


contains


   function new_AddExtCollectionMessage(template) result(message)
      type (AddExtCollectionMessage) :: message
      character(len=*), intent(in) :: template

      message%template = template
   end function new_AddExtCollectionMessage

   
   integer function get_type_id() result(type_id)
      type_id = ADDEXTCOLLECTION_ID
   end function get_type_id


   integer function get_length(this) result(length)
      class (AddExtCollectionMessage), intent(in) :: this
      length = serialize_buffer_length(this%template)
   end function get_length


   subroutine serialize(this, buffer, rc)
      class (AddExtCollectionMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:) ! no-op
      integer, optional, intent(out) :: rc
      buffer = serialize_intrinsic(this%template)
      _RETURN(_SUCCESS)
   end subroutine serialize


   subroutine deserialize(this, buffer, rc)
      class (AddExtCollectionMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      call deserialize_intrinsic(buffer, this%template)
      _RETURN(_SUCCESS)
   end subroutine deserialize

end module pFIO_AddExtCollectionMessageMod

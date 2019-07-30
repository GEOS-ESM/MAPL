#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_WaitRequestDataMessageMod
   use pFIO_ErrorHandlingMod
   use pFIO_AbstractMessageMod
   implicit none
   private

   public :: WaitRequestDataMessage

   type, extends(AbstractMessage) :: WaitRequestDataMessage
      integer :: request_id
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type WaitRequestDataMessage


   interface WaitRequestDataMessage
      module procedure new_WaitRequestDataMessage
   end interface WaitRequestDataMessage

contains

   function new_WaitRequestDataMessage(request_id) result(message)
      type (WaitRequestDataMessage) :: message
      integer, intent(in) :: request_id

      message%request_id = request_id

   end function new_WaitRequestDataMessage

   integer function get_type_id() result(type_id)
      type_id = WaitRequestData_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (WaitRequestDataMessage), intent(in) :: this
      length = 1
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (WaitRequestDataMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:) 
      integer, optional, intent(out) :: rc
      buffer = [this%request_id]
      _RETURN(_SUCCESS)

   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (WaitRequestDataMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      this%request_id = buffer(1)
      _RETURN(_SUCCESS)
   end subroutine deserialize
   
end module pFIO_WaitRequestDataMessageMod


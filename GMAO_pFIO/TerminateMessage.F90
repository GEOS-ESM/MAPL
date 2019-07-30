#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_TerminateMessageMod
   use pFIO_ErrorHandlingMod
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: TerminateMessage

   type, extends(AbstractMessage) :: TerminateMessage
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type TerminateMessage

   interface TerminateMessage
      module procedure new_TerminateMessage
   end interface

contains

   function new_TerminateMessage() result(message)
      type (TerminateMessage) :: message
      _UNUSED_DUMMY(message)
   end function new_TerminateMessage

   integer function get_type_id() result(type_id)
      type_id = TERMINATE_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (TerminateMessage), intent(in) :: this
      length = 0
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (TerminateMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) 
      integer, optional, intent(out) :: rc
      integer :: empty(0)
      buffer = empty
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (TerminateMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
   end subroutine deserialize
   
end module pFIO_TerminateMessageMod


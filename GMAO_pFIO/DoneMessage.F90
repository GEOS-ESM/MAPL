#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_DoneMessageMod
   use pFIO_ErrorHandlingMod
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: DoneMessage

   type, extends(AbstractMessage) :: DoneMessage
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type DoneMessage

   interface DoneMessage
      module procedure new_DoneMessage
   end interface

contains

   ! The Intel 17 compiler's default constructor for this class appears to be
   ! broken.   Possibly due to the extension of an abstract base class.
   function new_DoneMessage() result(message)
      type (DoneMessage) :: message
      _UNUSED_DUMMY(message)
   end function new_DoneMessage

   integer function get_type_id() result(type_id)
      type_id = DONE_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (DoneMessage), intent(in) :: this
      _UNUSED_DUMMY(this)
      length = 0
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (DoneMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) 
      integer, optional, intent(out) :: rc
      integer :: empty(0)

      _UNUSED_DUMMY(this)

      buffer = empty
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (DoneMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(buffer)
      _RETURN(_SUCCESS)
   end subroutine deserialize
   
end module pFIO_DoneMessageMod


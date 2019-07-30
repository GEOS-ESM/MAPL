#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_IDMessageMod
   use pFIO_ErrorHandlingMod
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: IDMessage

   type, extends(AbstractMessage) :: IDMessage
      integer :: id
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type IDMessage

   interface IDMessage
      module procedure new_IDMessage
   end interface IDMessage

contains

   function new_IDMessage(id) result(message)
      type (IDMessage) :: message
      integer, intent(in) :: id

      message%id = id

   end function new_IDMessage

   integer function get_type_id() result(type_id)
      type_id = ID_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (IDMessage), intent(in) :: this
      _UNUSED_DUMMY(this)
      length = 1
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (IDMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) 
      integer, optional, intent(out) :: rc
  
      buffer = [this%id]
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (IDMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      this%id = buffer(1)
      _RETURN(_SUCCESS)
   end subroutine deserialize
   
end module pFIO_IDMessageMod

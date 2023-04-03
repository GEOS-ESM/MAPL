#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_HandShakeMessageMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: HandShakeMessage

   type, extends(AbstractMessage) :: HandShakeMessage
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type HandShakeMessage

   interface HandShakeMessage
      module procedure new_HandShakeMessage
   end interface

contains

   function new_HandShakeMessage() result(message)
      type (HandShakeMessage) :: message
      return
      _UNUSED_DUMMY(message)
   end function new_HandShakeMessage

   integer function get_type_id() result(type_id)
      type_id = HandShake_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (HandShakeMessage), intent(in) :: this
      length = 0
      return
      _UNUSED_DUMMY(this) 
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (HandShakeMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) ! no-op
      integer, optional, intent(out) :: rc
      integer :: empty(0)
      buffer = empty
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (HandShakeMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(buffer)
   end subroutine deserialize
   
end module pFIO_HandShakeMessageMod

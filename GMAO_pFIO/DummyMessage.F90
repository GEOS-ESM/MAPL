#include "unused_dummy.H"

module pFIO_DummyMessageMod
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: DummyMessage

   type, extends(AbstractMessage) :: DummyMessage
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type DummyMessage

   interface DummyMessage
      module procedure new_DummyMessage
   end interface

contains

   function new_DummyMessage() result(message)
      type (DummyMessage) :: message
      _UNUSED_DUMMY(message)
   end function new_DummyMessage

   integer function get_type_id() result(type_id)
      type_id = DUMMY_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (DummyMessage), intent(in) :: this
      length = 0
   end function get_length

   subroutine serialize(this, buffer)
      class (DummyMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) ! no-op
      integer :: empty(0)
      buffer = empty
   end subroutine serialize

   subroutine deserialize(this, buffer)
      class (DummyMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)
   end subroutine deserialize
   
end module pFIO_DummyMessageMod


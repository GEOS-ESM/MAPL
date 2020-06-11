#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_StageDoneMessageMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: StageDoneMessage

   type, extends(AbstractMessage) :: StageDoneMessage
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type StageDoneMessage

   interface StageDoneMessage
      module procedure new_StageDoneMessage
   end interface

contains

   ! The Intel 17 compiler's default constructor for this class appears to be
   ! broken.   Possibly due to the extension of an abstract base class.
   function new_StageDoneMessage() result(message)
      type (StageDoneMessage) :: message
      return
      _UNUSED_DUMMY(message)
   end function new_StageDoneMessage

   integer function get_type_id() result(type_id)
      type_id = StageDone_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (StageDoneMessage), intent(in) :: this
      length = 0
      return
      _UNUSED_DUMMY(this)
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (StageDoneMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) 
      integer, optional, intent(out) :: rc
      integer :: empty(0)

      buffer = empty
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (StageDoneMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(buffer)
   end subroutine deserialize
   
end module pFIO_StageDoneMessageMod


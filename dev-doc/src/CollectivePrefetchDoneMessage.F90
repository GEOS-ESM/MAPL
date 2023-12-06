#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_CollectivePrefetchDoneMessageMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: CollectivePrefetchDoneMessage

   type, extends(AbstractMessage) :: CollectivePrefetchDoneMessage
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type CollectivePrefetchDoneMessage

   interface CollectivePrefetchDoneMessage
      module procedure new_CollectivePrefetchDoneMessage
   end interface

contains

   ! The Intel 17 compiler's default constructor for this class appears to be
   ! broken.   Possibly due to the extension of an abstract base class.
   function new_CollectivePrefetchDoneMessage() result(message)
      type (CollectivePrefetchDoneMessage) :: message
      return
      _UNUSED_DUMMY(message)
   end function new_CollectivePrefetchDoneMessage

   integer function get_type_id() result(type_id)
      type_id = CollectivePrefetchDone_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (CollectivePrefetchDoneMessage), intent(in) :: this
      length = 0
      return
      _UNUSED_DUMMY(this)
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (CollectivePrefetchDoneMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) 
      integer, optional, intent(out) :: rc
      integer :: empty(0)

      buffer = empty
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (CollectivePrefetchDoneMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(buffer)
   end subroutine deserialize
   
end module pFIO_CollectivePrefetchDoneMessageMod


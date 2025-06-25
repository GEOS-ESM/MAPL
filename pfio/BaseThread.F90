#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_BaseThreadMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_MessageVisitorMod
   use pfio_base
   use mpi

   implicit none
   private

   public :: BaseThread

   
   type, extends(MessageVisitor),abstract :: BaseThread
      private
      class (AbstractSocket), allocatable :: connection
      type (IntegerRequestMap) :: open_requests
   contains
   
      procedure :: get_connection
      procedure :: set_connection

      procedure :: erase_RequestHandle
      procedure :: clear_RequestHandle
      procedure :: get_RequestHandle
      procedure :: insert_RequestHandle
      procedure :: isEmpty_RequestHandle
   end type BaseThread

contains

   function get_connection(this, rc) result(connection)
      class (BaseThread), target, intent(in) :: this
      class (AbstractSocket), pointer :: connection
      integer, optional, intent(out) :: rc
      _assert(allocated(this%connection), "no connection")
      connection => this%connection
      _return(_success)
   end function get_connection

   subroutine set_connection(this, connection, rc)
      class(BaseThread),target,intent(inout) :: this
      class (AbstractSocket), intent(in) :: connection
      integer, optional, intent(out) :: rc

      if(allocated(this%connection)) deallocate(this%connection)
      allocate(this%connection, source=connection)

      _return(_success)
   end subroutine set_connection
   
   function get_RequestHandle(this,request_id, rc) result(rh_ptr)
      class (BaseThread), target, intent(in) :: this
      integer, intent(in) :: request_id
      integer, optional, intent(out) :: rc
      class(AbstractRequestHandle), pointer :: rh_ptr
      type (IntegerRequestMapIterator) :: iter

      iter = this%open_requests%find(request_id)
      _assert( iter /= this%open_requests%end(), "could not find the request handle id")
      rh_Ptr => iter%value()
      _return(_success)
   end function get_RequestHandle

   function isEmpty_RequestHandle(this, rc) result(empty)
      class (BaseThread), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      logical :: empty
      type (IntegerRequestMapIterator) :: iter

      iter  = this%open_requests%begin()
      empty = (iter == this%open_requests%end())
      _return(_success)
   end function isEmpty_RequestHandle

   subroutine insert_RequestHandle(this,request_id, handle, rc) 
      class (BaseThread), target, intent(inout) :: this
      integer, intent(in) :: request_id
      class(AbstractRequestHandle), intent(in):: handle
      integer, optional, intent(out) :: rc

      call this%open_requests%insert(request_id, handle)

      _return(_success)
   end subroutine insert_RequestHandle

   subroutine erase_RequestHandle(this,request_id, rc)
      class(BaseThread), target, intent(inout) :: this
      integer, intent(in) :: request_id
      integer, optional, intent(out) :: rc
      type(IntegerRequestMapIterator) :: iter

      iter = this%open_requests%find(request_id)
      call  this%open_requests%erase(iter)

      _return(_success)
   end subroutine erase_RequestHandle

   subroutine clear_RequestHandle(this, rc)
      class(BaseThread), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      class(AbstractRequestHandle), pointer :: rh_ptr
      type (IntegerRequestMapIterator) :: iter
      integer :: status

      iter = this%open_requests%begin()
      do while (iter /= this%open_requests%end())
        rh_ptr => iter%value()
        call rh_ptr%wait()
        call rh_ptr%data_reference%deallocate(status)
        _verify(status)

        call this%open_requests%erase(iter)
        iter = this%open_requests%begin()
      enddo

      _return(_success)
   end subroutine clear_RequestHandle

end module pFIO_BaseThreadMod

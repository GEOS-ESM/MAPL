#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_BaseThreadMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_MessageVisitorMod
   use pfio_base
   use, intrinsic :: iso_fortran_env, only: REAL32
   use mpi

   implicit none
   private

   public :: BaseThread

   integer, save :: GLOBAL_COUNTER = 0
   
   type, extends(MessageVisitor),abstract :: BaseThread
      private
      class (AbstractSocket), allocatable :: connection
      type (IntegerRequestMap) :: open_requests
      integer :: id = 0
   contains
   
      procedure :: get_connection
      procedure :: set_connection

      procedure :: erase_RequestHandle
      procedure :: clear_RequestHandle
      procedure :: get_RequestHandle
      procedure :: insert_RequestHandle
      procedure :: get_id
      procedure :: get_num
   end type BaseThread

contains

   function get_connection(this, rc) result(connection)
      class (BaseThread), target, intent(in) :: this
      class (AbstractSocket), pointer :: connection
      integer, optional, intent(out) :: rc
      _ASSERT(allocated(this%connection), "no connection")
      connection => this%connection
      _RETURN(_SUCCESS)
   end function get_connection

   subroutine set_connection(this, connection, rc)
      class(BaseThread),target,intent(inout) :: this
      class (AbstractSocket), intent(in) :: connection
      integer, optional, intent(out) :: rc

      GLOBAL_COUNTER = GLOBAL_COUNTER + 1
      this%id = GLOBAL_COUNTER
      _HERE,'id: ', this%id
      if(allocated(this%connection)) deallocate(this%connection)
      allocate(this%connection, source=connection)
      _RETURN(_SUCCESS)
   end subroutine set_connection
   
   function get_RequestHandle(this,request_id, rc) result(rh_ptr)
      class (BaseThread), target, intent(in) :: this
      integer, intent(in) :: request_id
      integer, optional, intent(out) :: rc
      class(AbstractRequestHandle), pointer :: rh_ptr
      type (IntegerRequestMapIterator) :: iter

      _HERE, 'id: ', this%id, this%open_requests%size()
      iter = this%open_requests%find(request_id)
      _ASSERT( iter /= this%open_requests%end(), "could not find the request handle id")
      rh_Ptr => iter%second()
      _HERE, 'id: ', this%id, this%open_requests%size()

      _RETURN(_SUCCESS)
   end function get_RequestHandle

   subroutine insert_RequestHandle(this,request_id, handle, rc) 
      class (BaseThread), target, intent(inout) :: this
      integer, intent(in) :: request_id
      class(AbstractRequestHandle), intent(in):: handle
      integer, optional, intent(out) :: rc

      _HERE, 'id: ', this%id, this%open_requests%size(), request_id
      call this%open_requests%insert(request_id, handle)
      _HERE, 'id: ', this%id, this%open_requests%size()

      _RETURN(_SUCCESS)
   end subroutine insert_RequestHandle

   subroutine erase_RequestHandle(this,request_id, rc)
      class(BaseThread), target, intent(inout) :: this
      integer, intent(in) :: request_id
      integer, optional, intent(out) :: rc
      type(IntegerRequestMapIterator) :: iter

      _HERE, 'id: ', this%id, this%open_requests%size()
      iter = this%open_requests%find(request_id)
      iter = this%open_requests%erase(iter)
      _HERE, 'id: ', this%id, this%open_requests%size()

      _RETURN(_SUCCESS)
   end subroutine erase_RequestHandle

   subroutine clear_RequestHandle(this, rc)
      class(BaseThread), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      class(AbstractRequestHandle), pointer :: rh_ptr
      type (IntegerRequestMapIterator) :: iter
      integer :: status

      _HERE
      _HERE,'**************'
      _HERE, 'clearing id: ', this%id, this%open_requests%size()
      iter = this%open_requests%begin()
      do while (iter /= this%open_requests%end())
         rh_ptr => iter%second()
         call rh_ptr%wait()
         call rh_ptr%data_reference%deallocate(status)
         _VERIFY(status)

         iter = this%open_requests%erase(iter)
      enddo
      _HERE, 'id: ', this%id, this%open_requests%size()
      _HERE,'**************'
      _HERE

      _RETURN(_SUCCESS)
   end subroutine clear_RequestHandle

   integer function get_id(this) result(id)
      class(BaseThread), intent(in) :: this
      id = this%id
   end function get_id

   integer function get_num(this) result(num)
      class(BaseThread), intent(in) :: this
      num = this%open_requests%size()
   end function get_num
end module pFIO_BaseThreadMod

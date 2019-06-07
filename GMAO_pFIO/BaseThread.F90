module pFIO_BaseThreadMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_MessageVisitorMod

   use, intrinsic :: iso_fortran_env, only: REAL32
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
      procedure :: get_RequestHandle
      procedure :: insert_RequestHandle

   end type BaseThread

contains

   function get_connection(this) result(connection)
      class (BaseThread), target, intent(in) :: this
      class (AbstractSocket), pointer :: connection
      connection => this%connection
   end function get_connection

   subroutine set_connection(this,connection)
      class(BaseThread),target,intent(inout) :: this
      class (AbstractSocket), intent(in) :: connection
  
      if(allocated(this%connection)) deallocate(this%connection)
      allocate(this%connection, source=connection)
  
   end subroutine set_connection
   
   function get_RequestHandle(this,request_id) result(rh_ptr)
      class (BaseThread),target, intent(in) :: this
      integer, intent(in) :: request_id
      class(AbstractRequestHandle), pointer :: rh_ptr
      type (IntegerRequestMapIterator) :: iter

      iter = this%open_requests%find(request_id)
      if(iter == this%open_requests%end()) stop "could not find the handle"
      rh_Ptr => iter%value()

   end function get_RequestHandle

   subroutine insert_RequestHandle(this,request_id,handle) 
      class (BaseThread),target,intent(inout) :: this
      integer, intent(in) :: request_id
      class(AbstractRequestHandle),intent(in):: handle

      call this%open_requests%insert(request_id, handle)

   end subroutine insert_RequestHandle

   subroutine erase_RequestHandle(this,request_id)
      class (BaseThread),target, intent(inout) :: this
      integer, intent(in) :: request_id
      type (IntegerRequestMapIterator) :: iter

      iter = this%open_requests%find(request_id)
      call  this%open_requests%erase(iter)

   end subroutine erase_RequestHandle

end module pFIO_BaseThreadMod

! Implements a socket with direct procedure call.
! 
! Data transfers are direct; put_data() immediately fills request.
! Asynchronous sockets will need additional interfaces to manage
! checking completion.

module pFIO_SimpleSocketMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractMessageMod
   use pFIO_DummyMessageMod
   use pFIO_BaseThreadMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_AbstractDataReferenceMod

   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   implicit none
   private

   public :: SimpleSocket

   type, extends (AbstractSocket) :: SimpleSocket
      private
      class (BaseThread), pointer :: visitor
   contains
      procedure :: receive
      procedure :: send
      procedure :: put
      procedure :: get
      procedure :: to_string
   end type SimpleSocket

   ! private type
   type, extends(AbstractRequestHandle) :: SimpleHandle
   contains
      procedure :: wait
   end type SimpleHandle
   
   interface SimpleSocket
      module procedure new_SimpleSocket_visitor
      module procedure new_SimpleSocket
   end interface SimpleSocket

   interface SimpleHandle
      module procedure new_SimpleHandle
   end interface SimpleHandle

contains

   function new_SimpleHandle(reference) result(handle)
      type(SimpleHandle) :: handle
      class(AbstractDataReference),intent(in) :: reference
      allocate(handle%data_reference,source=reference)
   end function new_SimpleHandle

   function new_SimpleSocket_visitor(visitor) result(socket)
      type (SimpleSocket), target :: socket
      class (BaseThread), target, intent(in) :: visitor

      socket%visitor => visitor

   end function new_SimpleSocket_visitor

   function new_SimpleSocket() result(socket)
      type (SimpleSocket), target :: socket
      socket%visitor => null()
   end function new_SimpleSocket

   function receive(this) result(message)
      class (AbstractMessage), pointer:: message
      class (SimpleSocket), intent(inout) :: this

      allocate(message, source=DummyMessage())

   end function receive

   recursive subroutine send(this, message)
      class (SimpleSocket), intent(inout) :: this
      class (AbstractMessage), intent(in) :: message
      
      call message%dispatch(this%visitor)

   end subroutine send

   function put(this, request_id, local_reference) result(handle)
      class (SimpleSocket), intent(inout) :: this
      class (AbstractRequestHandle), allocatable :: handle
      integer, intent(in) :: request_id
      class (AbstractDataReference), intent(in) :: local_reference
      class(AbstractRequestHandle),pointer :: visitor_handle

      visitor_handle =>this%visitor%get_RequestHandle(request_id)
      call local_reference%copy_data_to(visitor_handle%data_reference)

      allocate(handle, source=SimpleHandle(local_reference))

   end function put
      
   function get(this, request_id, local_reference) result(handle)
      class (AbstractRequestHandle), allocatable :: handle
      class (SimpleSocket), intent(inout) :: this
      class (AbstractDataReference), intent(in) :: local_reference
      integer, intent(in) :: request_id

      allocate(handle, source=SimpleHandle(local_reference))

   end function get

   subroutine wait(this)
      class (SimpleHandle), intent(inout) :: this
   end subroutine wait
   
   function to_string(this) result(string)
      class (SimpleSocket), intent(in) :: this
      character(len=:), allocatable :: string
      string = 'SimpleSocket::info'
   end function to_string

end module pFIO_SimpleSocketMod

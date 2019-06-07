#include "unused_dummy.H"
module MockSocketMod
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use pFIO_AbstractMessageMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_MessageQueueMod
   use pFIO_AbstractSocketMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_AddCollectionMessageMod
   use pFIO_CollectionIdMessageMod
   use pFIO_RequestIdMessageMod
   use pFIO_RequestDataMessageMod
   use pFIO_WaitRequestDataMessageMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_ArrayReferenceMod


   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private

   public :: MockSocket
   public :: MockSocketLog

   type :: MockSocketLog
      integer :: counter = 0
      character(len=:), allocatable :: log
      real(kind=REAL32), allocatable :: values_a
      real(kind=REAL32), allocatable :: values_u(:,:)
      real(kind=REAL32), allocatable :: values_v(:,:)
   end type MockSocketLog

   type, extends(AbstractSocket) :: MockSocket
      type (MessageQueue) :: messages
      type (MockSocketLog), pointer :: log
      integer :: collection_counter = 0
      real(kind=REAL32), allocatable :: q1
      real(kind=REAL32), allocatable :: q2(:)
   contains
      procedure :: prefix
      procedure :: receive
      procedure :: send
      procedure :: add_message
      procedure :: put
      procedure :: get
      procedure :: to_string
   end type MockSocket

   type, extends(AbstractRequestHandle) :: MockHandle
      type (MockSocket), pointer :: owner => null()
   contains
      procedure :: wait
   end type MockHandle

   interface MockSocket
      module procedure new_MockSocket
   end interface MockSocket

   interface MockHandle
      module procedure new_MockHandle
   end interface MockHandle

   
contains

   function new_MockHandle(owner) result(h)
      type (MockHandle) :: h
      type (MockSocket), target :: owner

      h%owner => owner

   end function new_MockHandle

   function new_MockSocket(log) result(socket)
      type (MockSocket) :: socket
      type (MockSocketLog), target :: log
      socket%log => log
   end function new_MockSocket

   subroutine prefix(this, string)
      class (MockSocket), intent(inout) :: this
      character(len=*), intent(in) :: string

      if (allocated(this%log%log)) then
         this%log%log = this%log%log // ' :: ' // string
      else
         this%log%log = string
      end if

   end subroutine prefix

   subroutine add_message(this, message)
      use pFIO_TerminateMessageMod
      class (MockSocket), intent(inout) :: this
      class (AbstractMessage), intent(in) :: message

      call this%messages%push_back(message)

   end subroutine add_message

   function receive(this) result(message)
      class (AbstractMessage), pointer :: message
      class (MockSocket), intent(inout) :: this

      type (MessageQueueIterator) :: iter

      if (this%messages%size() > 0) then
         allocate(message, source= this%messages%front())
         iter = this%messages%begin()
         call this%messages%erase(iter)

         select type (message)
         type is (TerminateMessage)
            call this%prefix('receive<Terminate>')
         type is (DoneMessage)
            call this%prefix('receive<Done>')
         type is (AddCollectionMessage)
            call this%prefix("receive<AddCollection('"//message%template//"')>")  
         type is (RequestDataMessage)
            call this%prefix("receive<RequestData('"//message%var_name//"')>")  
         end select
      else
         message => null()
      end if
      
   end function receive


   subroutine send(this, message)
      class (MockSocket), intent(inout) :: this
      class (AbstractMessage), intent(in) :: message

      character(len=100) :: buffer

      select type (message)
      type is (CollectionIdMessage)
         write(buffer,'("(",i3.3,")")') message%collection_id
         call this%prefix('send<CollectionId'//trim(buffer)//'>')
      type is (RequestIdMessage)
         write(buffer,'("(",i0,")")') message%request_id
         call this%prefix('send<RequestId'//trim(buffer)//'>')
      type is (AddCollectionMessage)
         call this%prefix("send<AddCollection('" // message%template // "')>")
         this%collection_counter = this%collection_counter + 1
         call this%messages%push_back(CollectionIdMessage(this%collection_counter))
      type is (RequestDataMessage)
         call this%prefix("send<RequestData('"//message%var_name//"')>")  
      type is (WaitRequestDataMessage)
         write(buffer,'("(",i0,")")') message%request_id
         call this%prefix('send<Wait'//trim(buffer)//'>')
      class default
         call this%prefix('send<unknown>')
      end select

   end subroutine send

   
   function put(this, request_id, local_reference) result(handle)
      class (AbstractRequestHandle), allocatable :: handle
      class (MockSocket), intent(inout) :: this
      integer, intent(in) :: request_id
      Class(AbstractDataReference), intent(in) :: local_reference

      real(kind=REAL32), pointer :: values_0d
      real(kind=REAL32), pointer :: values_2d(:,:)
      
      call this%prefix('put()')

      this%log%counter = this%log%counter + 1
      select case (this%log%counter)
      case (1)
         call c_f_pointer(local_reference%base_address, values_0d)
         this%log%values_a = values_0d
      case (2)
         call c_f_pointer(local_reference%base_address, values_2d, shape=local_reference%shape)
         this%log%values_u = values_2d
      case (3)
         call c_f_pointer(local_reference%base_address, values_2d, shape=local_reference%shape)
         this%log%values_v = values_2d
      end select

      allocate(handle, source=MockHandle(this))

   end function put
      
   function get(this, request_id, local_reference) result(handle)
      class (AbstractRequestHandle), allocatable :: handle
      class (MockSocket), intent(inout) :: this
      integer, intent(in) :: request_id
      class (AbstractDataReference), intent(in) :: local_reference

      real(kind=REAL32), pointer :: values_0d
      real(kind=REAL32), pointer :: values_1d(:)
      real(kind=REAL32), pointer :: values_2d(:,:)


      call this%prefix('get()')
      allocate(handle, source=MockHandle(this))
      this%log%counter = this%log%counter + 1

      select case (this%log%counter)
      case (1)
         call c_f_pointer(local_reference%base_address, values_0d)
         values_0d = this%q1
      case (2)
         call c_f_pointer(local_reference%base_address, values_1d, shape=local_reference%shape)
         values_1d = this%q2
      end select

   end function get

   subroutine wait(this)
      class (MockHandle), intent(inout) :: this
      call this%owner%prefix('wait()')
   end subroutine wait

   function to_string(this) result(string)
      class (MockSocket), intent(in) :: this
      character(len=:), allocatable :: string

      string = 'MockSocket::info'
   end function to_string


end module MockSocketMod

module pFIO_ClientThreadMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_MessageVisitorMod
   use pFIO_BaseThreadMod
   use pFIO_ArrayReferenceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_SimpleSocketMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_AddCollectionMessageMod
   use pFIO_CollectionIdMessageMod
   use pFIO_RequestIdMessageMod
   use pFIO_RequestDataMessageMod
   use pFIO_CollectiveRequestDataMessageMod
   use pFIO_WaitRequestDataMessageMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_DirectoryServiceMod

   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private

   public :: ClientThread

   integer, parameter :: MIN_ID = 1000
   integer, parameter :: MAX_ID = 9999

   integer, parameter :: COLLECTIVE_MIN_ID = 10000
   integer, parameter :: COLLECTIVE_MAX_ID = 19999

   
   type, extends(BaseThread) :: ClientThread
      private

      ! scratch pad for return values from application level interfaces
      integer :: collection_id = -1
      integer :: request_counter    = MIN_ID
      integer :: collective_counter = COLLECTIVE_MIN_ID

      ! temporary state items for internal communication
      !TODO: these should not bee class components.  Delete ...!
      ! deleted by WY
      !type (ArrayReference) :: data_reference
      !class (AbstractDirectoryService), pointer :: directory_service => null()
   contains
      procedure :: init_connection
      procedure :: add_collection
      procedure :: request_subset_data_reference ! Generic version
      procedure :: collective_request_data 
      procedure :: request_subset_0d
      procedure :: request_subset_1d
      procedure :: request_subset_2d
      generic :: request_subset => request_subset_0d
      generic :: request_subset => request_subset_1d
      generic :: request_subset => request_subset_2d

      procedure :: done
      procedure :: wait
      procedure :: terminate

      procedure :: handle_CollectionId

      procedure :: get_unique_request_id
      procedure :: get_unique_collective_request_id
   end type ClientThread


   interface ClientThread
      module procedure new_ClientThread
   end interface ClientThread

contains

   function new_ClientThread(sckt) result(c)
      type (ClientThread),target :: c
      class(AbstractSocket),optional,intent(in) :: sckt
  
      if(present(sckt)) call c%set_connection(sckt)

   end function new_ClientThread

   subroutine init_connection(this,ds,comm)
      class(ClientThread),target,intent(inout) :: this
      class(AbstractDirectoryService),target,intent(inout) :: ds
      integer,intent(in) :: comm
      type(PortInfo) :: ptinfo

      call this%set_connection(ds%connect_to_server(Portinfo('i_server',this), comm))

   end subroutine init_connection

   subroutine handle_CollectionId(this, message)
      class (ClientThread), intent(inout) :: this
      type (CollectionIdMessage), intent(in) :: message

      this%collection_id = message%collection_id
      
   end subroutine handle_CollectionId

   function add_collection(this, template) result(collection_id)
      integer :: collection_id
      class (ClientThread), intent(inout) :: this
      character(len=*), intent(in) :: template

      class (AbstractMessage), pointer :: message
      class(AbstractSocket),pointer :: connection

      connection=>this%get_connection()
      call connection%send(AddCollectionMessage(template))

      message => connection%receive()

      call message%dispatch(this)

      collection_id = this%collection_id

   end function add_collection


   function request_subset_data_reference(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start) result(request_id)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      type (ArrayReference) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, intent(in), optional :: start(:)

      integer :: request_id
      class (AbstractMessage), pointer :: message
      integer :: empty(0)
      class(AbstractSocket),pointer :: connection

      request_id = this%get_unique_request_id()
      connection=>this%get_connection()
      call connection%send(RequestDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference, start=start))

      message => connection%receive()

      deallocate(message)

      associate (id => request_id)
        call this%insert_RequestHandle(id, connection%get(id, data_reference))
      end associate

   end function request_subset_data_reference

   function collective_request_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start,global_start,global_count) result(request_id)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      type (ArrayReference) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, intent(in), optional :: start(:)
      integer, intent(in), optional :: global_start(:)
      integer, intent(in), optional :: global_count(:)

      integer :: request_id

      class (AbstractMessage), pointer :: message
      integer :: empty(0)
      class(AbstractSocket),pointer :: connection

      request_id = this%get_unique_collective_request_id()
      connection => this%get_connection()
      call connection%send(CollectiveRequestDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference, start=start,&
           global_start=global_start,global_count=global_count))

      message => connection%receive()

      deallocate(message)

      associate (id => request_id)
        call this%insert_RequestHandle(id, connection%get(id, data_reference))
      end associate

   end function collective_request_data

   function request_subset_0d(this, collection_id, file_name, var_name, array) result(request_id)
      integer :: request_id
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (*), target :: array

      request_id = this%request_subset_data_reference(collection_id, file_name, var_name, ArrayReference(array))
      
   end function request_subset_0d


   function request_subset_1d(this, collection_id, file_name, var_name, array, start) result(request_id)
      integer :: request_id
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class(*), target :: array(:)
      integer, optional, intent(in) :: start(:)

      request_id = this%request_subset_data_reference(collection_id, file_name, var_name, &
                        ArrayReference(array), start=start)
      
   end function request_subset_1d


   function request_subset_2d(this, collection_id, file_name, var_name, array, start) result(request_id)
      integer :: request_id
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class(*), target :: array(:,:)
      integer, optional, intent(in) :: start(:)

      request_id = this%request_subset_data_reference(collection_id, file_name, var_name, &
                      ArrayReference(array), start=start)
      
   end function request_subset_2d


   ! Tell server that ClientThread is done making new requests for the
   ! moment.  This allows the server to be more responsive during the
   ! requests phase of operations.
   subroutine done(this)
      class (ClientThread), intent(inout) :: this
      class(AbstractSocket),pointer :: connection

      connection=>this%get_connection()
      call connection%send(DoneMessage())
   end subroutine done

   subroutine wait(this, request_id)
      use pFIO_AbstractRequestHandleMod
      class (ClientThread), target, intent(inout) :: this
      integer, intent(in) :: request_id
      class(AbstractRequestHandle), pointer :: handle
      class(AbstractSocket),pointer :: connection

      connection=>this%get_connection()
      call connection%send(WaitRequestDataMessage(request_id))
      handle => this%get_RequestHandle(request_id)
      call handle%wait()
      call this%erase_RequestHandle(request_id)

   end subroutine wait

   integer function get_unique_request_id(this) result(request_id)
      class (ClientThread), intent(inout) :: this

      associate (n => this%request_counter)
        n = MIN_ID + mod(n +1 - MIN_ID, (MAX_ID-MIN_ID+1))
      end associate

      request_id = this%request_counter
   end function get_unique_request_id

   integer function get_unique_collective_request_id(this) result(request_id)
      class (ClientThread), intent(inout) :: this

      associate (n => this%collective_counter)
        n = COLLECTIVE_MIN_ID + mod(n +1 - COLLECTIVE_MIN_ID, (COLLECTIVE_MAX_ID-COLLECTIVE_MIN_ID+1))
      end associate

      request_id = this%collective_counter
   end function get_unique_collective_request_id

   subroutine terminate(this)
      class (ClientThread), intent(inout) :: this
      class(AbstractSocket),pointer :: connection

      connection=>this%get_connection()
      call connection%send(TerminateMessage())
   end subroutine terminate

end module pFIO_ClientThreadMod

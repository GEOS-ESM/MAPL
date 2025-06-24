#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ClientThreadMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_MessageVisitorMod
   use pFIO_BaseThreadMod
   use pFIO_AbstractDataReferenceMod
   use mapl_KeywordEnforcerMod
   use pFIO_SimpleSocketMod
   use pFIO_FileMetadataMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_DummyMessageMod
   use pFIO_HandShakeMessageMod
   use pFIO_PrefetchDoneMessageMod
   use pFIO_CollectivePrefetchDoneMessageMod
   use pFIO_StageDoneMessageMod
   use pFIO_CollectiveStageDoneMessageMod
   use pFIO_AddExtCollectionMessageMod
   use pFIO_AddHistCollectionMessageMod
   use pFIO_IdMessageMod
   use pFIO_PrefetchDataMessageMod
   use pFIO_StageDataMessageMod
   use pFIO_CollectivePrefetchDataMessageMod
   use pFIO_CollectiveStageDataMessageMod
   use pFIO_ModifyMetadataMessageMod
   use pFIO_ReplaceMetadataMessageMod
   use pFIO_StringVariableMapMod

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
      integer :: collection_id      = -1
      integer :: request_counter    = MIN_ID
      integer :: collective_counter = COLLECTIVE_MIN_ID

   contains
      procedure :: add_ext_collection
      procedure :: add_hist_collection
      procedure :: modify_metadata
      procedure :: replace_metadata
      procedure :: prefetch_data
      procedure :: stage_data
      procedure :: collective_prefetch_data
      procedure :: collective_stage_data
      procedure :: stage_nondistributed_data
      procedure :: shake_hand

      procedure :: done
      procedure :: done_prefetch
      procedure :: done_collective_prefetch
      procedure :: done_stage
      procedure :: done_collective_stage
      procedure :: wait
      procedure :: wait_all
      procedure :: post_wait_all
      procedure :: terminate

      procedure :: handle_Id

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

   subroutine handle_Id(this, message, rc)
      class (ClientThread), intent(inout) :: this
      type (IdMessage), intent(in) :: message
      integer, optional, intent(out) :: rc
      !this%collection_id = message%id
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(message)
   end subroutine handle_Id

   function add_ext_collection(this, template, rc) result(collection_id)
      integer :: collection_id
      class (ClientThread), intent(inout) :: this
      character(len=*), intent(in) :: template
      integer, optional, intent(out) :: rc

      class (AbstractMessage), pointer :: message
      class(AbstractSocket),pointer :: connection
      integer :: status

      connection=>this%get_connection()
      call connection%send(AddExtCollectionMessage(template),__RC)
      message => connection%receive()
      select type(message)
      type is(IDMessage)
        collection_id = message%id
      class default
        __FAIL( " should get id message")
      end select
      __RETURN(__SUCCESS)
   end function add_ext_collection

   function add_hist_collection(this, fmd, unusable,  mode, rc) result(hist_collection_id)
      integer :: hist_collection_id
      class (ClientThread), target, intent(inout) :: this
      type(FileMetadata),intent(in) :: fmd
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: mode
      integer, optional, intent(out) :: rc

      class (AbstractMessage), pointer :: message
      class(AbstractSocket), pointer :: connection

      connection=>this%get_connection()
      call connection%send(AddHistCollectionMessage(fmd, mode=mode))

      message => connection%receive()
      select type(message)
      type is(IDMessage)
        hist_collection_id = message%id
      class default
        __FAIL( " should get id message")
      end select

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function add_hist_collection

   function prefetch_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start, rc) result(request_id)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in)  :: start(:)
      integer, optional, intent(out) :: rc

      integer :: request_id
      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      integer :: status

      request_id = this%get_unique_request_id()
      connection=>this%get_connection()
      call connection%send(PrefetchDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference,unusable=unusable,start=start),__RC)

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      associate (id => request_id)
        ! the get call iRecv
        call this%insert_RequestHandle(id, connection%get(id, data_reference))
      end associate
      __RETURN(__SUCCESS)
   end function prefetch_data

   subroutine modify_metadata(this, collection_id, unusable,var_map, rc)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      class (KeywordEnforcer), optional, intent(out) :: unusable
      type (StringVariableMap), optional,intent(in) :: var_map
      integer, optional, intent(out) :: rc

      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      integer :: status

      connection=>this%get_connection()
      call connection%send(ModifyMetadataMessage( &
           collection_id, &
           var_map=var_map),__RC)

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end subroutine modify_metadata

   subroutine replace_metadata(this, collection_id, fmd, rc)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      type (FileMetadata),intent(in) :: fmd
      integer, optional, intent(out) :: rc

      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      integer :: status

      connection=>this%get_connection()
      call connection%send(ReplaceMetadataMessage(collection_id,fmd),__RC)

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      __RETURN(__SUCCESS)
   end subroutine replace_metadata

   function collective_prefetch_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start,global_start,global_count, rc) result(request_id)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)
      integer, optional, intent(out):: rc

      integer :: request_id

      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      integer :: status

      request_id = this%get_unique_collective_request_id()
      connection => this%get_connection()

      call connection%send(CollectivePrefetchDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference,unusable=unusable, start=start,&
           global_start=global_start,global_count=global_count),__RC)

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      associate (id => request_id)
        ! the get call iRecv
        call this%insert_RequestHandle(id, connection%get(id, data_reference))
      end associate

      __RETURN(__SUCCESS)
   end function collective_prefetch_data

   function stage_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start, rc) result(request_id)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in)  :: start(:)
      integer, optional, intent(out) :: rc

      integer :: request_id
      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      integer :: status

      request_id = this%get_unique_request_id()
      connection=>this%get_connection()
      call connection%send(StageDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference,unusable=unusable,start=start),__RC)

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      associate (id => request_id)
        ! the put call iSend
        call this%insert_RequestHandle(id, connection%put(id, data_reference))
      end associate
      __RETURN(__SUCCESS)
   end function stage_data

   function collective_stage_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start,global_start,global_count, rc) result(request_id)
      class (ClientThread), target, intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)
      integer, optional, intent(out) :: rc

      integer :: request_id

      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      integer :: status

      request_id = this%get_unique_collective_request_id()
      connection => this%get_connection()

      call connection%send(CollectiveStageDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference,unusable=unusable, start=start,&
           global_start=global_start,global_count=global_count),__RC)

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      associate (id => request_id)
        ! the put call iSend
        call this%insert_RequestHandle(id, connection%put(id, data_reference))
      end associate

      __RETURN(__SUCCESS)
   end function collective_stage_data

   function stage_nondistributed_data(this, collection_id, file_name, var_name, data_reference, rc) result(request_id)
      class (ClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      integer, optional, intent(out) :: rc


      integer :: request_id

      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection

      request_id = this%get_unique_collective_request_id()
      connection => this%get_connection()
      call connection%send(CollectiveStageDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference))

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      associate (id => request_id)
        ! the put call iSend
        call this%insert_RequestHandle(id, connection%put(id, data_reference))
      end associate
      __RETURN(__SUCCESS)
   end function stage_nondistributed_data

   subroutine shake_hand(this, rc)
      class (ClientThread), intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractSocket),pointer :: connection

      class (AbstractMessage), pointer :: handshake_msg
      integer :: status

      connection=>this%get_connection()
      call connection%send(HandShakeMessage(),__RC)

      handshake_msg => connection%receive()
      deallocate(handshake_msg)

      __RETURN(__SUCCESS)
   end subroutine shake_hand
   ! Tell server that ClientThread is done making new requests for the
   ! moment.  This allows the server to be more responsive during the
   ! requests phase of operations.
   subroutine done(this, rc)
      class (ClientThread), intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractSocket),pointer :: connection
      integer :: status

      connection=>this%get_connection()
      call connection%send(DoneMessage(),__RC)
      __RETURN(__SUCCESS)
   end subroutine done

   subroutine done_prefetch(this, rc)
      class (ClientThread), intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractSocket),pointer :: connection
      integer :: status

      if (this%isEmpty_RequestHandle()) then
        __RETURN(__SUCCESS)
      endif
    
      connection=>this%get_connection()
      call connection%send(PrefetchDoneMessage(),__RC)
      __RETURN(__SUCCESS)
   end subroutine done_prefetch

   subroutine done_collective_prefetch(this, rc)
      class (ClientThread), intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractSocket),pointer :: connection
      integer :: status
  
      if (this%isEmpty_RequestHandle()) then
        __RETURN(__SUCCESS)
      endif

      connection=>this%get_connection()
      call connection%send(CollectivePrefetchDoneMessage(),__RC)
      __RETURN(__SUCCESS)
   end subroutine done_collective_prefetch

   subroutine done_stage(this, rc)
      class (ClientThread), intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractSocket),pointer :: connection
      integer :: status

      if (this%isEmpty_RequestHandle()) then
        __RETURN(__SUCCESS)
      endif

      connection=>this%get_connection()
      call connection%send(StageDoneMessage(),__RC)
      __RETURN(__SUCCESS)
   end subroutine done_stage

   subroutine done_collective_stage(this, rc)
      class (ClientThread), intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractSocket),pointer :: connection
      integer :: status

      if (this%isEmpty_RequestHandle()) then
        __RETURN(__SUCCESS)
      endif
     
      connection=>this%get_connection()
      call connection%send(CollectiveStageDoneMessage(),__RC)
      __RETURN(__SUCCESS)
   end subroutine done_collective_stage

   subroutine wait(this, request_id, rc)
      use pFIO_AbstractRequestHandleMod
      class (ClientThread), target, intent(inout) :: this
      integer, intent(in) :: request_id
      integer, optional, intent(out) :: rc
      integer :: status
      class(AbstractRequestHandle), pointer :: handle

      handle => this%get_RequestHandle(request_id)
      call handle%wait()
      call handle%data_reference%deallocate()
      call this%erase_RequestHandle(request_id)
      __RETURN(__SUCCESS)
  
   end subroutine wait

   subroutine wait_all(this, rc)
      use pFIO_AbstractRequestHandleMod
      class (ClientThread), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer:: status
      call this%clear_RequestHandle(__RC)
      !call this%shake_hand()
      __RETURN(__SUCCESS)

   end subroutine wait_all

   subroutine post_wait_all(this, rc)
      use pFIO_AbstractRequestHandleMod
      class (ClientThread), target, intent(inout) :: this
      integer, optional, intent(out):: rc
      integer :: status
      call this%wait_all(__RC)
      __RETURN(__SUCCESS)
   end subroutine post_wait_all

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

   subroutine terminate(this, rc)
      class (ClientThread), intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractSocket),pointer :: connection
      integer :: status

      connection=>this%get_connection()
      call connection%send(TerminateMessage(),__RC)
      __RETURN(__SUCCESS)
   end subroutine terminate

end module pFIO_ClientThreadMod

#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_FastClientThreadMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_LocalMemReferenceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_ClientThreadMod
   use pFIO_StageDataMessageMod
   use pFIO_CollectiveStageDataMessageMod
   implicit none
   private

   public :: FastClientThread

   type, extends(ClientThread) :: FastClientThread
   contains
      procedure :: stage_data
      procedure :: collective_stage_data 
      procedure :: stage_nondistributed_data
      procedure :: post_wait_all
   end type FastClientThread


   interface FastClientThread
      module procedure new_FastClientThread
   end interface FastClientThread

contains

   function new_FastClientThread(sckt) result(c)
      class(AbstractSocket),optional,intent(in) :: sckt
      type (FastClientThread),target :: c
      if (present(sckt)) call c%set_connection(sckt)
   end function new_FastClientThread

   function stage_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start, rc) result(request_id)
      class (FastClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in)  :: start(:)
      integer, optional, intent(out) :: rc

      integer :: request_id, status
      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      type (LocalMemReference) :: mem_data_reference 

      request_id = this%get_unique_request_id()
      connection=>this%get_connection()
      call connection%send(StageDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference,unusable=unusable,start=start))

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      associate (id => request_id)
         mem_data_reference = LocalMemReference(data_reference%type_kind, data_reference%shape)
         ! copy data out so the client can move on after done message is send
         call data_reference%copy_data_to(mem_data_reference, rc=status)
         _VERIFY(status) 
         ! put calls iSend
         call this%insert_RequestHandle(id, connection%put(id, mem_data_reference))
      end associate
      _RETURN(_SUCCESS)
   end function stage_data

   function collective_stage_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start,global_start,global_count, rc) result(request_id)
      class (FastClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)
      integer, optional, intent(out) :: rc

      integer :: request_id, status

      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      type (LocalMemReference) :: mem_data_reference

      request_id = this%get_unique_collective_request_id()
      connection => this%get_connection()

      call connection%send(CollectiveStageDataMessage( &
           request_id, &
           collection_id, &
           file_name, &
           var_name, &
           data_reference,unusable=unusable, start=start,&
           global_start=global_start,global_count=global_count))

      handshake_msg => connection%receive()
      deallocate(handshake_msg)
      associate (id => request_id)
         mem_data_reference = LocalMemReference(data_reference%type_kind, data_reference%shape)
         ! copy data out so the client can move on after done message is send
         call data_reference%copy_data_to(mem_data_reference, rc=status)
         _VERIFY(status) 
         ! put calls iSend
         call this%insert_RequestHandle(id, connection%put(id, mem_data_reference))
      end associate

      _RETURN(_SUCCESS)
   end function collective_stage_data

   function stage_nondistributed_data(this, collection_id, file_name, var_name, data_reference, rc) result(request_id)
      class (FastClientThread), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      integer, optional, intent(out) :: rc


      integer :: request_id, status

      class (AbstractMessage), pointer :: handshake_msg
      class(AbstractSocket),pointer :: connection
      type (LocalMemReference) :: mem_data_reference

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
         mem_data_reference = LocalMemReference(data_reference%type_kind, data_reference%shape)
         ! copy data out so the client can move on after done message is send
         call data_reference%copy_data_to(mem_data_reference, rc=status)
         _VERIFY(status) 
         ! put calls iSend
         call this%insert_RequestHandle(id, connection%put(id, mem_data_reference))
      end associate

      _RETURN(_SUCCESS)
   end function stage_nondistributed_data

   ! The data has been copied out and post no wait after isend
   subroutine post_wait_all(this)
      use pFIO_AbstractRequestHandleMod
      class (FastClientThread), target, intent(inout) :: this
      ! do nothing on purpose
   end subroutine post_wait_all

end module pFIO_FastClientThreadMod

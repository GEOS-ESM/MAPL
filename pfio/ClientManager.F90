#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ClientManagerMod

   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use pFIO_AbstractDataReferenceMod
   use pFIO_FileMetadataMod
   use pFIO_ClientThreadMod
   use pFIO_FastClientThreadMod
   use pFIO_StringVariableMapMod

   implicit none
   private

   public :: ClientManager
   public :: init_IO_ClientManager
   public :: i_Client
   public :: o_Client

   type :: ClientManager
     private
     integer :: client_comm, rank
     class(ClientThread), allocatable :: client
   contains
      procedure, private :: add_read_data_collection
      procedure, private :: add_write_data_collection
      generic :: add_data_collection => add_read_data_collection, add_write_data_collection
      procedure :: modify_metadata
      procedure :: replace_metadata
      procedure :: modify_metadata_all
      procedure :: replace_metadata_all
      procedure :: prefetch_data
      procedure :: stage_data
      procedure :: collective_prefetch_data
      procedure :: collective_stage_data
      procedure :: stage_nondistributed_data
      procedure :: shake_hand

      procedure :: done_prefetch
      procedure :: done_collective_prefetch
      procedure :: done_stage
      procedure :: done_collective_stage
      procedure :: wait
      procedure :: post_wait
      procedure :: terminate

      procedure :: get_client_thread
   end type

   interface ClientManager
      module procedure new_ClientManager
   end interface

   interface init_IO_ClientManager
      module procedure init_ClientManager
   end interface

   type (ClientManager), target :: i_Client
   type (ClientManager), target :: o_Client

contains

   function new_ClientManager(client_comm, unusable, fast_oclient, rc) result(c_manager)
      integer, intent(in) :: client_comm
      class (KeywordEnforcer), optional, intent(out) :: unusable
      logical, optional, intent(in) :: fast_oclient
      integer, optional, intent(out) :: rc
      type (ClientManager) :: c_manager

      logical :: fast_

      fast_ = .false.
      if (present(fast_oclient)) fast_ = fast_oclient

      if (fast_) then
         allocate(c_manager%client, source=FastClientThread())
      else
         allocate(c_manager%client, source=ClientThread())
      end if

      c_manager%client_comm = client_comm
      call MPI_Comm_rank(client_comm, c_manager%rank, rc)
      _VERIFY(rc)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function new_ClientManager

   function add_read_data_collection(this, file_template, unusable, rc) result(collection_id)
      integer :: collection_id
      class (ClientManager), intent(inout) :: this
      character(len=*), intent(in) :: file_template
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      collection_id = this%client%add_data_collection(file_template)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function add_read_data_collection

   function add_write_data_collection(this, file_metadata, unusable, mode, rc) result(collection_id)
      integer :: collection_id
      class (ClientManager), intent(inout) :: this
      type(FileMetadata), intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: mode
      integer, optional, intent(out) :: rc

      collection_id = this%client%add_data_collection(file_metadata, mode=mode)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function add_write_data_collection

   subroutine prefetch_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in)  :: start(:)
      integer, optional, intent(out) :: rc

      integer :: request_id, status

      request_id = this%client%prefetch_data(collection_id, file_name, var_name, data_reference, start=start, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(request_id)
   end subroutine prefetch_data

   subroutine modify_metadata(this, collection_id, unusable, var_map, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      class (KeywordEnforcer), optional, intent(out) :: unusable
      type (StringVariableMap), optional, intent(in) :: var_map
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client%modify_metadata(collection_id, var_map=var_map, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine modify_metadata

   subroutine replace_metadata(this, collection_id, fmd, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      type (FileMetadata), intent(in) :: fmd
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client%replace_metadata(collection_id, fmd, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine replace_metadata

   subroutine modify_metadata_all(this, collection_id, unusable, var_map, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      class (KeywordEnforcer), optional, intent(out) :: unusable
      type (StringVariableMap), optional, intent(in) :: var_map
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client%modify_metadata(collection_id, var_map=var_map, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine modify_metadata_all

   subroutine replace_metadata_all(this, collection_id, fmd, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      type (FileMetadata), intent(in) :: fmd
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client%replace_metadata(collection_id, fmd, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine replace_metadata_all

   subroutine collective_prefetch_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start, global_start, global_count, rc)
      class (ClientManager), intent(inout) :: this
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

      request_id = this%client%collective_prefetch_data(collection_id, file_name, var_name, data_reference, &
                   start=start, global_start=global_start, global_count=global_count, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(request_id)
   end subroutine collective_prefetch_data

   subroutine stage_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in)  :: start(:)
      integer, optional, intent(out) :: rc

      integer :: request_id, status

      request_id = this%client%stage_data(collection_id, file_name, var_name, data_reference, start=start, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(request_id)
   end subroutine stage_data

   subroutine collective_stage_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start, global_start, global_count, rc)
      class (ClientManager), intent(inout) :: this
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

      request_id = this%client%collective_stage_data(collection_id, file_name, var_name, data_reference, &
                   start=start, global_start=global_start, global_count=global_count, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(request_id)
   end subroutine collective_stage_data

   subroutine stage_nondistributed_data(this, collection_id, file_name, var_name, data_reference, unusable, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: request_id, status

      request_id = this%client%collective_stage_data(collection_id, file_name, var_name, data_reference, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(request_id)
   end subroutine stage_nondistributed_data

   subroutine shake_hand(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      call this%client%shake_hand(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine shake_hand

   subroutine done_prefetch(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      call this%client%done_prefetch(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_prefetch

   subroutine done_collective_prefetch(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      call this%client%done_collective_prefetch(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_collective_prefetch

   subroutine done_stage(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      call this%client%done_stage(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_stage

   subroutine done_collective_stage(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client%done_collective_stage(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_collective_stage

   subroutine wait(this, unusable, rc)
      class (ClientManager), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      call this%client%wait_all(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine wait

   subroutine post_wait(this, unusable, rc)
      class (ClientManager), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client%post_wait_all(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine post_wait

   subroutine terminate(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client%wait_all(_RC)
      call this%client%terminate()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine terminate

   function get_client_thread(this) result(clientPtr)
      class (ClientManager), target, intent(in) :: this
      class (ClientThread), pointer :: clientPtr
      clientPtr => this%client
   end function get_client_thread

   subroutine init_ClientManager(client_comm, unusable, fast_oclient, rc)
      integer, intent(in) :: client_comm
      class (KeywordEnforcer), optional, intent(out) :: unusable
      logical, optional, intent(in) :: fast_oclient
      integer, optional, intent(out) :: rc
      integer :: status

      i_Client = ClientManager(client_comm, rc=status)
      _VERIFY(status)
      o_Client = ClientManager(client_comm, fast_oclient=fast_oclient, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine init_ClientManager

end module pFIO_ClientManagerMod

#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ClientManagerMod

   use MAPL_ExceptionHandling
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_FileMetadataMod
   use pFIO_ClientThreadMod
   use pFIO_ClientThreadVectorMod
   use pFIO_StringVariableMapMod
   use gFTL_IntegerVector

   implicit none
   private

   public :: ClientManager

   type :: ClientManager
     private
     integer :: current_client = 1
     type(ClientThreadVector) :: clients
     type(IntegerVector) :: server_sizes
     type(IntegerVector) :: large_server_pool
     type(IntegerVector) :: small_server_pool
     integer :: smallCurrent=1
     integer :: largeCurrent=1
     integer :: writeCutoff
   contains
      procedure :: add_ext_collection
      procedure :: add_hist_collection
      procedure :: replace_hist_collection
      procedure :: modify_metadata
      procedure :: modify_metadata_all
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
      procedure :: terminate

      procedure :: size
      procedure :: next
      procedure :: current
      procedure :: set_current
      procedure :: set_ideal_client
      procedure :: split_clients
      procedure :: set_server_size
   end type

   interface ClientManager
      module procedure new_ClientManager
   end interface

contains

   function new_ClientManager(unusable, n_client, rc) result (c_manager)
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: n_client
      integer, optional, intent(out) :: rc
      type (ClientManager) :: c_manager

      class (ClientThread), pointer :: clientPtr
      integer :: i, n

      n = 1
      if (present(n_client)) n = n_client
      c_manager%clients = ClientThreadVector()     
      do i = 1, n
        allocate(clientPtr, source = ClientThread())
        call c_manager%clients%push_back(clientPtr)
        clientPtr=>null()
      enddo
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function new_ClientManager

   function add_ext_collection(this, template, unusable, rc) result(collection_id)
      integer :: collection_id
      class (ClientManager), intent(inout) :: this
      character(len=*), intent(in) :: template
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      type (ClientThread), pointer :: clientPtr

      integer :: i

      do i = 1, this%size()
         ClientPtr => this%clients%at(i)
         collection_id = clientPtr%add_ext_collection(template)
      enddo

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function add_ext_collection

   function add_hist_collection(this, fmd, unusable, rc) result(hist_collection_id)
      integer :: hist_collection_id
      class (ClientManager), intent(inout) :: this
      type(FileMetadata),intent(in) :: fmd
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      type (ClientThread), pointer :: clientPtr
      integer :: i
      
      do i = 1, this%size()
         ClientPtr => this%clients%at(i)
         hist_collection_id = clientPtr%add_hist_collection(fmd)
      enddo

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function add_hist_collection

   subroutine replace_hist_collection(this, hist_collection_id, fmd, unusable, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: hist_collection_id
      type(FileMetadata),intent(in) :: fmd
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr
      integer :: i

      do i = 1, this%size()
         ClientPtr => this%clients%at(i)
         call clientPtr%replace_hist_collection(hist_collection_id, fmd)
      enddo

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine replace_hist_collection

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

      type (ClientThread), pointer :: clientPtr
      integer :: request_id, status

      clientPtr =>this%current()
      request_id = clientPtr%prefetch_data(collection_id, file_name, var_name, data_reference, start=start, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine prefetch_data

   subroutine modify_metadata(this, collection_id, unusable,var_map, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      class (KeywordEnforcer), optional, intent(out) :: unusable
      type (StringVariableMap), optional,intent(in) :: var_map
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr
      integer :: status
   
      ClientPtr => this%current()
      call clientPtr%modify_metadata(collection_id, var_map = var_map, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine modify_metadata

   subroutine modify_metadata_all(this, collection_id, unusable,var_map,rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      class (KeywordEnforcer), optional, intent(out) :: unusable
      type (StringVariableMap), optional,intent(in) :: var_map
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr
      integer :: i, status

      do i = 1, this%clients%size()
         ClientPtr => this%clients%at(i)
         call clientPtr%modify_metadata(collection_id, var_map = var_map, rc=status)
         _VERIFY(status)
      enddo

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine modify_metadata_all

   subroutine collective_prefetch_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start,global_start,global_count, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)
      integer, optional, intent(out):: rc

      type(clientThread), pointer :: clientPtr
      integer :: request_id, status

      clientPtr =>this%current()
      request_id = clientPtr%collective_prefetch_data(collection_id, file_name, var_name, data_reference, start=start, &
                   global_start = global_start, global_count=global_count, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
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

      type(clientThread), pointer :: clientPtr
      integer :: request_id, status

      clientPtr =>this%current()
      request_id = clientPtr%stage_data(collection_id, file_name, var_name, data_reference, start=start, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine stage_data

   subroutine collective_stage_data(this, collection_id, file_name, var_name, data_reference, &
        & unusable, start,global_start,global_count, rc)
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

      type(clientThread), pointer :: clientPtr
      integer :: request_id, status

      clientPtr =>this%current()
      request_id = clientPtr%collective_stage_data(collection_id, file_name, var_name, data_reference, start=start, global_start=global_start, &
                   global_count=global_count, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine collective_stage_data

   subroutine stage_nondistributed_data(this, collection_id, file_name, var_name, data_reference, unusable, rc) 
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type(clientThread), pointer :: clientPtr
      integer :: request_id, status

      clientPtr =>this%current()
      request_id = clientPtr%collective_stage_data(collection_id, file_name, var_name, data_reference, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine stage_nondistributed_data

   subroutine shake_hand(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr

      clientPtr =>this%current()
      call clientPtr%shake_hand()
 
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine shake_hand

   subroutine done_prefetch(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr

      clientPtr =>this%current()
      call clientPtr%done_prefetch()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_prefetch

   subroutine done_collective_prefetch(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr
   
      clientPtr =>this%current()
      call clientPtr%done_collective_prefetch()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_collective_prefetch

   subroutine done_stage(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr
   
      clientPtr =>this%current()
      call clientPtr%done_stage()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_stage

   subroutine done_collective_stage(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr
   
      clientPtr =>this%current()
      call clientPtr%done_collective_stage()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine done_collective_stage

   subroutine wait(this, unusable, rc)
      class (ClientManager), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
   
      type (ClientThread), pointer :: clientPtr
      
      clientPtr =>this%current()
      call clientPtr%wait_all()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine wait

   subroutine terminate(this, unusable, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      type (ClientThread), pointer :: clientPtr
      integer :: i

      do i = 1, this%size()
         clientPtr =>this%clients%at(i)
         call clientPtr%terminate()
      enddo

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine terminate

   subroutine next(this)
      class (ClientManager), target,intent(inout) :: this
      this%current_client = this%current_client + 1
      if (this%current_client > this%clients%size()) this%current_client = 1
   end subroutine next

   subroutine set_current(this, ith, rc) 
      class (ClientManager), intent(inout) :: this
      integer, optional, intent(in) :: ith
      integer, optional, intent(out) :: rc
      integer :: ith_
      ith_ = 1
      if (present(ith)) ith_ = ith
      _ASSERT( 1<=ith_ .and. ith_<=this%size(), "exceeding the clients number")
      this%current_client = ith_
      _RETURN(_SUCCESS)
   end subroutine set_current

   function current(this) result(clientPtr) 
      class (ClientManager), target, intent(in) :: this
      type (ClientThread), pointer :: clientPtr
      clientPtr=> this%clients%at(this%current_client)
   end function current

   subroutine set_ideal_client(this,nwriting,unusable,rc) 
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: nwriting
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ! no "small" pool, just get next
      if (this%small_server_pool%size() == 0) then
         call this%next()
      else
         _ASSERT(this%large_server_pool%size()>0,'large server pool must be great than zero')
         _ASSERT(this%small_server_pool%size()>0,'small server pool must be great than zero')
         if (nwriting .ge. this%writeCutoff) then
            this%largeCurrent=this%largeCurrent+1
            if (this%largeCurrent .gt. this%large_server_pool%size()) this%largeCurrent=1
            call this%set_current( ith =  this%large_server_pool%at(this%largeCurrent))
         else
            this%smallCurrent=this%smallCurrent+1
            if (this%smallCurrent .gt. this%small_server_pool%size()) this%smallCurrent=1
            call this%set_current( ith = this%small_server_pool%at(this%smallCurrent) )
         end if
      end if
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_ideal_client

   subroutine split_clients(this, nsplit, nwriteCutoff,unusable, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: nsplit
      integer, intent(in) :: nWriteCutoff
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i

      do i=1,this%server_sizes%size()
          if (this%server_sizes%at(i) >= nsplit) then
             call this%large_server_pool%push_back(i)
          else
             call this%small_server_pool%push_back(i)
          end if
      enddo
      this%writeCutoff = nwriteCutoff
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine split_clients

   subroutine set_server_size(this, server_size, unusable, rc)
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: server_size
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call this%server_sizes%push_back(server_size)

   end subroutine set_server_size 

   function size(this) result(n_client) 
      class (ClientManager), intent(in) :: this
      integer :: n_client
      n_client = this%clients%size()
   end function size

end module pFIO_ClientManagerMod

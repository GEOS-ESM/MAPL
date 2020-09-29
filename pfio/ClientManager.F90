#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ClientManagerMod

   use MAPL_ExceptionHandling
   use MAPL_SortMod
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
   public :: init_IO_ClientManager
   public :: i_Clients
   public :: o_Clients

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
     integer :: large_total = 0
     integer :: small_total = 0
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
      procedure :: set_optimal_server
      procedure :: split_server_pools
      procedure :: set_server_size
   end type

   interface ClientManager
      module procedure new_ClientManager
   end interface

   interface init_IO_ClientManager
      module procedure init_ClientManager
   end interface

   type (ClientManager), target :: i_Clients
   type (ClientManager), target :: o_Clients

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

   subroutine set_optimal_server(this,nwriting,unusable,rc) 
      class (ClientManager), intent(inout) :: this
      integer, intent(in) :: nwriting
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer, save, allocatable :: nwritings(:) ! saved the past nwritings 
      integer, save, allocatable :: nwritings_large(:) ! saved the past large nwritings 
      integer, save, allocatable :: nwritings_small(:) ! saved the past small nwritings 
      integer :: Cuttoff, ssize, lsize, tsize
      integer, allocatable :: nwritings_order(:)
      real :: l_ratio, s_ratio

      ! if there is no "small" pool, then there is no "large" pool either, just get next
      ssize = this%small_server_pool%size()
      lsize = this%large_server_pool%size()
      tsize = this%server_sizes%size()

      if (ssize == 0) then
         call this%next()
         _RETURN(_SUCCESS)
      endif

      _ASSERT(lsize > 0,'large server pool must be great than zero')

      if (.not. allocated (nwritings)) allocate (nwritings(tsize), source = 0)
      if (.not. allocated (nwritings_large)) allocate (nwritings_large(lsize), source = 0)
      if (.not. allocated (nwritings_small)) allocate (nwritings_small(ssize), source = 0)

      Cuttoff = this%writeCutoff
      ! if writeCutoff is not set, then pick one from the past experience
      ! the whole pool size is used to determine how many past nwrting are saved
      if (this%writeCutoff == 0) then
         nwritings(2:tsize) = nwritings(1:tsize-1)
         nwritings(1)       = nwriting
         allocate(nwritings_order(tsize), source = nwritings(:))

         call MAPL_sort(nwritings_order)
         Cuttoff = nwritings_order(tsize/2)
         deallocate(nwritings_order)
      endif

      if (nwriting == Cuttoff) then ! after this block, nwrting /= Cuttoff
         l_ratio = sum(nwritings_large)/ (this%large_total*1.0)
         s_ratio = sum(nwritings_small)/ (this%small_total*1.0)

         if (s_ratio >= l_ratio ) then! .true. means small pool is busier
            Cuttoff = Cuttoff - 1 ! artificially decrease Cuttoff, so the next will go to large pool
         else 
            Cuttoff = Cuttoff + 1 ! artificially increase Cuttoff, so the next will go to small pool
         endif
      endif 

      if (nwriting > Cuttoff) then
         this%largeCurrent=this%largeCurrent+1
         if (this%largeCurrent .gt. lsize) this%largeCurrent=1
         call this%set_current( ith =  this%large_server_pool%at(this%largeCurrent))
         nwritings_large(1:lsize-1) = nwritings_large(2:lsize)
         nwritings_large(lsize) = nwriting
      endif

      if (nwriting < Cuttoff) then
         this%smallCurrent=this%smallCurrent+1
         if (this%smallCurrent .gt. this%small_server_pool%size()) this%smallCurrent=1
         call this%set_current( ith = this%small_server_pool%at(this%smallCurrent) )
         nwritings_small(1:ssize-1) = nwritings_small(2:ssize)
         nwritings_small(ssize) = nwriting
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_optimal_server

   subroutine split_server_pools(this, unusable, n_server_split, n_hist_split, rc)
      class (ClientManager), intent(inout) :: this
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(in) :: n_server_split
      integer, optional, intent(in) :: n_hist_split
      integer, optional, intent(out) :: rc

      integer :: i, nsplit, nwriteCutoff, tsize
      integer, allocatable :: server_sizes(:), tmp_position(:)
      integer :: pos

      tsize = this%server_sizes%size()
      if (tsize == 1) then
        _RETURN(_SUCCESS)
      endif

      nsplit = 0
      if (present(n_server_split)) nsplit = n_server_split

      allocate(server_sizes(tsize))
      allocate(tmp_position(tsize))
      do i=1,tsize
         server_sizes(i) = this%server_sizes%at(i)
         tmp_position(i) = i
      enddo
      call MAPL_Sort(server_sizes, tmp_position)
      ! if nsplit is out of scope, pick the mid point
      if (nsplit < server_sizes(1) .or. nsplit > server_sizes(tsize)) then 
         pos = tsize/2
      else
         pos = 0
         do i = 1, tsize
            if ( nsplit < server_sizes(i) ) then
               pos = i-1
               exit
            endif
         enddo
         if (pos == 0) pos = tsize/2
      endif

      this%large_total = 0
      this%small_total = 0

      do i = 1, pos
         call this%small_server_pool%push_back(tmp_position(i))
         this%small_total = this%small_total + this%server_sizes%at(tmp_position(i))
      enddo

      do i = pos + 1, tsize
         call this%large_server_pool%push_back(tmp_position(i))
         this%large_total = this%large_total + this%server_sizes%at(tmp_position(i))
      enddo  

      this%writeCutoff = 0
      if (present(n_hist_split)) this%writeCutoff = n_hist_split

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine split_server_pools

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

   subroutine init_ClientManager(unusable, n_i, n_o, rc)
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(in) :: n_i
      integer, optional, intent(in) :: n_o
      integer, optional, intent(out):: rc
      integer :: status

      i_Clients = ClientManager(n_client = n_i, rc=status)
      o_Clients = ClientManager(n_client = n_o, rc=status)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine init_ClientManager

end module pFIO_ClientManagerMod

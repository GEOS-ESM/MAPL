#include "MAPL_ErrLog.h"
#include "unused_dummy.H"


! A simple comm splitter determines the local color purely from the
! rank and npes in the shared communicator.  Its subclasses can be
! tested in a serial context by parameterizing rank and npes.

module MAPL_SimpleCommSplitterMod
   use MAPL_CommGroupDescriptionMod
   use MAPL_CommGroupDescriptionVectorMod
   use MAPL_ExceptionHandling
   use MAPL_AbstractCommSplitterMod
   use MAPL_KeywordEnforcerMod
   use MAPL_SplitCommunicatorMod
   use MPI
   implicit none
   
   private
   public :: SimpleCommSplitter

   type, extends(AbstractCommSplitter) :: SimpleCommSplitter
      private
      character(:), allocatable :: base_name
      type (CommGroupDescriptionVector) :: group_descriptions
   contains
      procedure :: split
      procedure :: add_group_simple
      generic :: add_group => add_group_simple
      procedure :: compute_color
      procedure :: get_node_sizes
      procedure :: get_node_id
   end type SimpleCommSplitter


   interface SimpleCommSplitter
      module procedure new_SimpleCommSplitter
      module procedure ensemble_comm_splitter
   end interface SimpleCommSplitter

contains


   function new_SimpleCommSplitter(communicator, unusable, base_name) result(splitter)
      type (SimpleCommSplitter) :: splitter
      integer, intent(in) :: communicator
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: base_name

      _UNUSED_DUMMY(unusable)
      call splitter%set_shared_communicator(communicator)

      if (present(base_name)) then
         splitter%base_name = base_name
      else
         splitter%base_name = ''
      end if
      
   end function new_SimpleCommSplitter

   function ensemble_comm_splitter(communicator, n_members, npes_member, unusable, isolate_nodes, base_name) result(splitter)
      type (SimpleCommSplitter) :: splitter
      integer, intent(in) :: communicator
      integer, intent(in) :: n_members
      integer, intent(in) :: npes_member
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: isolate_nodes
      character(*), optional, intent(in) :: base_name

      integer :: i
      _UNUSED_DUMMY(unusable)

      splitter = SimpleCommSplitter(communicator, base_name=base_name)
      do i = 1, n_members
         call splitter%add_group(npes=npes_member, isolate_nodes=isolate_nodes)
      end do
      
   end function ensemble_comm_splitter


   function split(this, unusable, rc) result(split_communicator)
      type (SplitCommunicator) :: split_communicator
      class (SimpleCommSplitter), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: color
      integer :: subcommunicator
      integer :: ierror
      integer :: status
      type (CommGroupDescription), pointer :: group_description => null()
      character(:), allocatable :: name

      _UNUSED_DUMMY(unusable)

      color = this%compute_color(rc=status)
      _VERIFY(status)
      
      call MPI_Comm_split(this%get_shared_communicator(), color, 0, subcommunicator, ierror)
      _VERIFY(ierror)

      if (subcommunicator == MPI_COMM_NULL) then
         _ASSERT(color == MPI_UNDEFINED, "color should not be defined")
         name = NULL_SUBCOMMUNICATOR_NAME
      else
         group_description => this%group_descriptions%at(color)
         name = group_description%name
      end if

      split_communicator = SplitCommunicator(subcommunicator, color, name)

      _RETURN(_SUCCESS)
   end function split

   subroutine add_group_simple(this, unusable, npes, nnodes, isolate_nodes, npes_per_node, name, rc)
      class (SimpleCommSplitter), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: npes
      integer, optional, intent(in) :: nnodes
      logical, optional, intent(in) :: isolate_nodes
      integer, optional, intent(in) :: npes_per_node
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      character(:), allocatable :: name_
      logical :: isolate_nodes_
      integer :: n, npes_,nnodes_, status

      character(24) :: buffer

      _UNUSED_DUMMY(unusable)

      if (present(name)) then
         name_ = name
      else
         n = this%group_descriptions%size() + 1
         write(buffer,'(i0)') n
         name_ = this%base_name // trim(buffer)
      end if

      isolate_nodes_ = .true.
      if (present(isolate_nodes)) then
         isolate_nodes_ = isolate_nodes
      endif
 
      npes_ = 0
      if (present(npes)) npes_ = npes

      nnodes_ = 0
      if (present(nnodes)) then
         nnodes_ = nnodes
         _ASSERT( nnodes_ ==0 .or. npes_ == 0, "npes and nnodes are exclusive")
      endif

      if (nnodes_ > 0) then
         _ASSERT(isolate_nodes, " nnodes should be isolated")
      endif

      call this%group_descriptions%push_back(CommGroupDescription(npes_, nnodes_, isolate_nodes_, name_, npes_per_node = npes_per_node, rc=status))
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine add_group_simple
   
   integer function compute_color(this, unusable, rc) result(color)
      class (SimpleCommSplitter), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: shared_communicator
      integer :: ierror
      integer :: color_
      integer :: node_id
      integer :: node_comm, rank_on_node, start_node, start_rank, next_node, next_rank
      integer, allocatable :: node_sizes(:)
      logical :: IamInGroup
      integer :: status

      type (CommGroupDescription), pointer :: group_descr
      integer :: info = MPI_INFO_NULL

      _UNUSED_DUMMY(unusable)


      ! Note that the shared communicator may not be ordered with pe's contiguous
      ! within a node.  Thus, the assignment algorithm must step through
      ! nodes and then through pe's within a node.   Messy - and will probably
      ! never be exercised in the nontrivial case.
      shared_communicator = this%get_shared_communicator()
      call MPI_Comm_split_type(shared_communicator, MPI_COMM_TYPE_SHARED, 0, info, node_comm, ierror)
      _VERIFY(ierror)
      call MPI_Comm_rank(node_comm, rank_on_node, ierror); _VERIFY(ierror)

      node_id = this%get_node_id(rc=status); _VERIFY(status)
      node_sizes = this%get_node_sizes(rc=status); _VERIFY(status)

      color = MPI_UNDEFINED ! unless ...

      start_node = 1
      start_rank = 0
      IamInGroup = .false.

      do color_ = 1, this%group_descriptions%size()

         group_descr => this%group_descriptions%at(color_)
         
         call group_descr%comm_group_range(node_id, rank_on_node, node_sizes, start_node, start_rank, next_node, next_rank, IamInGroup)
         
         start_node = next_node
         start_rank = next_rank

         if (IamInGroup) then
            color = color_
            exit
         endif
      enddo

      _RETURN(_SUCCESS)
      
   end function compute_color


   ! Nodes are numbered by the order of the node roots within the
   ! global communicator starting at 1. (Not zero!)
   integer function get_node_id(this, unusable, rc) result(node_id)
      class (SimpleCommSplitter), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: node_comm
      integer :: shared_communicator
      integer :: npes, rank, ierror
      integer :: status
      integer :: rank_on_node
      integer, allocatable :: node_ranks(:)
      integer :: info = MPI_INFO_NULL
      
      _UNUSED_DUMMY(unusable)

      shared_communicator = this%get_shared_communicator()
      call MPI_Comm_split_type(shared_communicator, MPI_COMM_TYPE_SHARED, 0, info, node_comm, ierror)
      _VERIFY(ierror)
      call MPI_Comm_size(shared_communicator, npes, ierror); _VERIFY(ierror)
      call MPI_Comm_rank(shared_communicator, rank, ierror); _VERIFY(ierror)
      call MPI_Comm_rank(node_comm, rank_on_node, ierror); _VERIFY(ierror)

      allocate(node_ranks(0:npes-1), stat=status);  _VERIFY(status)
      call MPI_Allgather(rank_on_node, 1, MPI_INTEGER, node_ranks, 1, MPI_INTEGER, shared_communicator, ierror)
      _VERIFY(ierror)

      if (rank_on_node == 0) then
         node_id = 1 + count(node_ranks(0:rank-1) == 0)  ! Numbering starts at _1_.
      end if

      ! Share node_id with other processes on same node
      call MPI_Bcast(node_id, 1, MPI_INTEGER, 0, node_comm, ierror)
      _VERIFY(ierror)

      _RETURN(_SUCCESS)

   end function get_node_id

   function get_node_sizes(this, unusable, rc) result(node_sizes)
      class (SimpleCommSplitter), intent(in) :: this
      integer, allocatable :: node_sizes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: npes, ierror
      integer :: node_comm
      integer :: shared_communicator
      integer :: status
      integer :: rank_on_node, npes_on_node
      integer :: info = MPI_INFO_NULL

      _UNUSED_DUMMY(unusable)
     
      shared_communicator = this%get_shared_communicator()
      call MPI_Comm_split_type(shared_communicator, MPI_COMM_TYPE_SHARED, 0, info, node_comm, ierror)
      _VERIFY(ierror)
 
      call MPI_Comm_size(shared_communicator, npes, ierror); _VERIFY(ierror)
      allocate(node_sizes(0:npes-1), stat=status);  _VERIFY(status)

      call MPI_Comm_rank(node_comm, rank_on_node, ierror); _VERIFY(ierror)
      if (rank_on_node == 0) then
         call MPI_Comm_size(node_comm, npes_on_node, ierror); _VERIFY(ierror)
      else
         npes_on_node = -1 ! do not use
      end if

      call MPI_Allgather(npes_on_node, 1, MPI_INTEGER, node_sizes, 1, MPI_INTEGER, shared_communicator, ierror)
      _VERIFY(ierror)

      node_sizes = pack(node_sizes, (node_sizes /= -1))

      _RETURN(_SUCCESS)
   end function get_node_sizes


end module MAPL_SimpleCommSplitterMod


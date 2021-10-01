#include "MAPL_Generic.h"
module AEIO_MpiConnection
   use MPI
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_SplitCommunicatorMod
   use MAPL_SimpleCommSplitterMod

   implicit none
   private
   
   public :: MpiConnection

   type MpiConnection
      integer :: connection_comm
      integer :: front_comm
      integer :: back_comm
      integer, allocatable :: front_mpi_ranks(:)
      integer, allocatable :: back_mpi_ranks(:)
      integer, allocatable :: front_pets(:)
      integer, allocatable :: back_pets(:)
      logical :: i_am_front_root
      logical :: i_am_back_root
   contains
      procedure :: get_connection_comm
      procedure :: get_front_comm
      procedure :: get_back_comm
      procedure :: get_front_mpi_ranks
      procedure :: get_back_mpi_ranks
      procedure :: get_front_pets
      procedure :: get_back_pets
      procedure :: am_i_front_root
      procedure :: am_i_back_root
   end type

   interface MpiConnection
      module procedure create_from_comm
      module procedure new_MpiConnection
   end interface

contains

   function create_from_comm(global_comm,comm,writers_per_node,vm,rc) result(new_instance)
      integer, intent(in) :: global_comm
      integer, intent(in) :: comm
      integer, intent(in) :: writers_per_node
      type(ESMF_VM), intent(in) :: vm
      integer, intent(out), optional :: rc

      type(MpiConnection) :: new_instance
      type(SimpleCommSplitter) :: splitter
      type (SplitCommunicator)   ::  c_comm
      integer :: c_size,front_size,back_size,c_pet,c_rank,local_rank,global_size
      integer :: status
      integer, allocatable :: node_sizes(:),front_mpi_ranks(:),back_mpi_ranks(:),front_pets(:),back_pets(:)
      character(len=:), allocatable :: c_name
      integer :: MPI_STAT(MPI_STATUS_SIZE)

      new_instance%I_am_front_root = .false.
      new_instance%I_am_back_root = .false.
      call MPI_COMM_SIZE(global_comm,global_size,status)
      _VERIFY(status)
      new_instance%connection_comm = comm 
      new_instance%front_comm = MPI_COMM_NULL
      new_instance%back_comm = MPI_COMM_NULL
      if (comm /= mpi_comm_null) then
         splitter = SimpleCommsplitter(new_instance%connection_comm)
         call MPI_COMM_SIZE(new_instance%connection_comm,c_size,status)
         _VERIFY(status)
         node_sizes = splitter%get_node_sizes()
         back_size = size(node_sizes)*writers_per_node
         front_size = c_size - back_size
         write(*,*) "bmaa size ",node_sizes,back_size,front_size
         allocate(new_instance%front_mpi_ranks(front_size))
         allocate(new_instance%back_mpi_ranks(back_size))
         allocate(new_instance%front_pets(front_size))
         allocate(new_instance%back_pets(back_size))
     
         if (c_size < node_sizes(1)) then
            call splitter%add_group(npes = c_size - writers_per_node, name="server_front", isolate_nodes=.false.)
            call splitter%add_group(npes = writers_per_node,          name="server_back",  isolate_nodes=.false.)
         else
            call splitter%add_group(npes_per_node = node_sizes(1)-writers_per_node, name="server_front", isolate_nodes=.false.)
            call splitter%add_group(npes_per_node = writers_per_node,               name="server_back",  isolate_nodes=.false.)
         endif

         c_comm = splitter%split(_RC)
         call MPI_Comm_rank(new_instance%connection_comm, c_rank,status)
         _VERIFY(status)
         call ESMF_VMGet(vm,localPet=c_pet,_RC)
         c_name = c_comm%get_name()

         if (index(c_name, 'server_front') /=0) then
            new_instance%front_comm = c_comm%get_subcommunicator()
            call MPI_Comm_rank(new_instance%front_comm, local_rank, status)
            if (c_rank == 0) then
              _ASSERT( local_rank == 0, "re-arrange the rank of the server_comm")
              new_instance%I_am_front_root = .true.
              call MPI_recv(new_instance%back_mpi_ranks, back_size, MPI_INTEGER, MPI_ANY_SOURCE, 666, new_instance%connection_comm, MPI_STAT,status)
              call MPI_recv(new_instance%back_pets, back_size, MPI_INTEGER, MPI_ANY_SOURCE, 666, new_instance%connection_comm, MPI_STAT,status)
            endif
            call MPI_Bcast(new_instance%back_mpi_ranks, back_size, MPI_INTEGER, 0, new_instance%front_comm, status)
            call MPI_Bcast(new_instance%back_pets, back_size, MPI_INTEGER, 0, new_instance%front_comm, status)

            call MPI_AllGather(c_rank, 1, MPI_INTEGER, new_instance%front_mpi_ranks, 1, MPI_INTEGER, new_instance%front_comm, status)
            call MPI_AllGather(c_pet, 1, MPI_INTEGER, new_instance%front_pets, 1, MPI_INTEGER, new_instance%front_comm, status)
            if (local_rank ==0 ) then
               call MPI_Send(new_instance%front_mpi_ranks, c_size-back_size, MPI_INTEGER, new_instance%back_mpi_ranks(1), 777, new_instance%connection_comm, status)
               call MPI_Send(new_instance%front_pets, c_size-back_size, MPI_INTEGER, new_instance%back_mpi_ranks(1), 777, new_instance%connection_comm, status)
            endif
         endif

         if (index(c_name, 'server_back') /=0) then
            new_instance%back_comm =c_comm%get_subcommunicator()
            call MPI_AllGather(c_rank, 1, MPI_INTEGER, new_instance%back_mpi_ranks, 1, MPI_INTEGER, new_instance%back_comm, status)
            call MPI_AllGather(c_pet, 1, MPI_INTEGER, new_instance%back_pets, 1, MPI_INTEGER, new_instance%back_comm, status)
            call MPI_Comm_rank(new_instance%back_comm, local_rank, status)
            if (local_rank ==0 ) then
               new_instance%I_am_back_root = .true.
               call MPI_Send(new_instance%back_mpi_ranks, back_size, MPI_INTEGER, 0, 666, new_instance%connection_comm, status)
               call MPI_Send(new_instance%back_pets, back_size, MPI_INTEGER, 0, 666, new_instance%connection_comm, status)
            endif

            if (c_rank == new_instance%back_mpi_ranks(1)) then
               _ASSERT( local_rank == 0, "re-arrange the rank of the server_comm")
               call MPI_recv(new_instance%front_mpi_ranks, front_size, MPI_INTEGER, MPI_ANY_SOURCE, 777, new_instance%connection_comm, MPI_STAT,status)
               call MPI_recv(new_instance%front_pets, front_size, MPI_INTEGER, MPI_ANY_SOURCE, 777, new_instance%connection_comm, MPI_STAT,status)
            endif

            call MPI_Bcast(new_instance%front_mpi_ranks, front_size, MPI_INTEGER, 0, new_instance%back_comm, status)
            call MPI_Bcast(new_instance%front_pets, front_size, MPI_INTEGER, 0, new_instance%back_comm, status)
         endif

      endif
      call MPI_Bcast(front_size,1,MPI_INTEGER,global_size-1,global_comm,status)
      _VERIFY(status)
      call MPI_Bcast(back_size,1,MPI_INTEGER,global_size-1,global_comm,status)
      _VERIFY(status)
      if (new_instance%connection_comm == MPI_COMM_NULL) then
         allocate(new_instance%front_mpi_ranks(front_size))
         allocate(new_instance%front_pets(front_size))
         allocate(new_instance%back_mpi_ranks(back_size))
         allocate(new_instance%back_pets(back_size))
      end if
      call mpi_bcast(new_instance%front_mpi_ranks,front_size,mpi_integer,global_size-1,global_comm,status)
      _VERIFY(status)
      call mpi_bcast(new_instance%front_pets,front_size,mpi_integer,global_size-1,global_comm,status)
      _VERIFY(status)
      call mpi_bcast(new_instance%back_mpi_ranks,back_size,mpi_integer,global_size-1,global_comm,status)
      _VERIFY(status)
      call mpi_bcast(new_instance%back_pets,back_size,mpi_integer,global_size-1,global_comm,status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function create_from_comm

   function new_MpiConnection(connection_comm,front_comm,back_comm,&
            front_mpi_ranks,back_mpi_ranks,front_pets,back_pets) result (new_instance)
      integer, intent(in) :: connection_comm
      integer, intent(in) :: front_comm
      integer, intent(in) :: back_comm
      integer, allocatable, intent(in) :: front_mpi_ranks(:)
      integer, allocatable, intent(in) :: back_mpi_ranks(:)
      integer, allocatable, intent(in) :: front_pets(:)
      integer, allocatable, intent(in) :: back_pets(:)

      type(MpiConnection) :: new_instance

      new_instance%connection_comm=connection_comm
      new_instance%front_comm=front_comm
      new_instance%back_comm=back_comm
      allocate(new_instance%front_mpi_ranks,source=front_mpi_ranks)
      allocate(new_instance%back_mpi_ranks,source=back_mpi_ranks)
      allocate(new_instance%front_pets,source=front_pets)
      allocate(new_instance%back_pets,source=back_pets)

   end function new_MpiConnection

   function get_connection_comm(this) result(connection_comm)
      class(MpiConnection), intent(in) :: this
      integer :: connection_comm

      connection_comm=this%connection_comm
   end function get_connection_comm

   function get_front_comm(this) result(front_comm)
      class(MpiConnection), intent(in) :: this
      integer :: front_comm

      front_comm=this%front_comm
   end function get_front_comm

   function get_back_comm(this) result(back_comm)
      class(MpiConnection), intent(in) :: this
      integer :: back_comm

      back_comm=this%back_comm
   end function get_back_comm

   function get_back_mpi_ranks(this) result(back_mpi_ranks)
      class(MpiConnection), intent(in) :: this
      integer, allocatable :: back_mpi_ranks(:)

      allocate(back_mpi_ranks,source=this%back_mpi_ranks)

   end function get_back_mpi_ranks 

   function get_front_mpi_ranks(this) result(front_mpi_ranks)
      class(MpiConnection), intent(in) :: this
      integer, allocatable :: front_mpi_ranks(:)

      allocate(front_mpi_ranks,source=this%front_mpi_ranks)

   end function get_front_mpi_ranks 

   function get_back_pets(this) result(back_pets)
      class(MpiConnection), intent(in) :: this
      integer, allocatable :: back_pets(:)

      allocate(back_pets,source=this%back_pets)

   end function get_back_pets 

   function get_front_pets(this) result(front_pets)
      class(MpiConnection), intent(in) :: this
      integer, allocatable :: front_pets(:)

      allocate(front_pets,source=this%front_pets)

   end function get_front_pets

   function am_i_front_root(this) result(i_am_front_root) 
      class(MpiConnection), intent(in) :: this
      logical :: i_am_front_root

      i_am_front_root = this%i_am_front_root

   end function am_i_front_root

   function am_i_back_root(this) result(i_am_back_root) 
      class(MpiConnection), intent(in) :: this
      logical :: i_am_back_root

      i_am_back_root = this%i_am_back_root

   end function am_i_back_root

end module

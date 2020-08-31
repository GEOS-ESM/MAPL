#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

! Motivation 1:   Server calls connect once per collective client.
! Motivation 2:   Server gets a new connection on each call to connect.

! These motivations are in conflict.
! Possible resolutions:   DS maintains a list of requests and connect() pulls from the list each time connect is called.
! Requires Queue-like behavior

! Motivation 3:  Server can control distribution of connections.
! Why?   (1) This allows the server to do some form of load balancing
!        (2) Allows variant strategies in which one server process satisfies an entire client.

module pFIO_DirectoryServiceMod
   use, intrinsic :: iso_c_binding, only: c_f_pointer, c_ptr, c_sizeof
   use MAPL_ExceptionHandling
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractServerMod
   use pFIO_ServerThreadMod
   use pFIO_BaseServerMod
   use pFIO_MpiMutexMod
   use pFIO_AbstractSocketMod
   use pFIO_SimpleSocketMod
   use pFIO_MpiSocketMod
   use pFIO_ProtocolParserMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_AbstractDirectoryServiceMod
   use mpi
   implicit none
   private

   public :: Directory
   public :: DirectoryEntry
   public :: DirectoryService

   integer, parameter :: TERMINATE = -9999

   ! MPI Tags
   integer, parameter :: DISCOVERY_TAG = 1 ! Exchange of _root_ rank between client and server
   integer, parameter :: NPES_TAG = 2  ! Client sends number of pes in client to server  (on roots)
   integer, parameter :: RANKS_TAG = 3 ! Client sends ranks of client processes to server (on roots)
   integer, parameter :: CONNECT_TAG = 3 ! client and server individual processes exchange ranks 

   type :: DirectoryEntry
      sequence
      character(len=MAX_LEN_PORT_NAME) :: port_name = ''
      integer :: partner_root_rank = -1
   end type DirectoryEntry

   type :: Directory
      sequence
      type (DirectoryEntry) :: entries(MAX_NUM_PORTS)
      integer :: num_entries = 0
   end type Directory

   type,extends(AbstractDirectoryService) :: DirectoryService
      private
      integer :: comm,rank
      type (MpiMutex) :: mutex
      integer :: win_server_directory
      integer :: win_client_directory
      type(c_ptr) :: server_dir
      type(c_ptr) :: client_dir
      type (ProtocolParser) :: parser
      ! TODO: make vector
      type (PortInfo) :: local_ports(MAX_NUM_PORTS)
      integer :: n_local_ports = 0
   contains
      procedure :: connect_to_server
      procedure :: connect_to_client

      procedure :: publish
      procedure :: terminate_servers

      procedure :: get_win
      procedure :: get_directory
      procedure :: put_directory
      procedure :: free_directory_resources
   end type DirectoryService

   interface DirectoryService
      module procedure new_DirectoryService
   end interface DirectoryService

contains

   function new_DirectoryService(comm, unusable, rc) result(ds)
      type (DirectoryService) :: ds
      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: ierror
      type (Directory) :: empty_dir

      call MPI_Comm_dup(comm, ds%comm, ierror)
      _VERIFY(ierror)
      call MPI_Comm_rank(ds%comm, ds%rank, ierror)
      _VERIFY(ierror)

      ! Create windows that will be used for coordination
      ! 1. lock - control modification of other windows
      ds%mutex = MpiMutex(ds%comm)
      ds%win_server_directory = make_directory_window(ds%comm, ds%server_dir)
      ds%win_client_directory = make_directory_window(ds%comm, ds%client_dir)

      if(ds%rank == 0) then
         call ds%put_directory(empty_dir, ds%win_client_directory)
         call ds%put_directory(empty_dir, ds%win_server_directory)
      end if

      ds%parser = ProtocolParser()
      ! Need to be sure that the directories have been initialized before
      ! proceeding
      call MPI_Barrier(comm, ierror)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function new_DirectoryService

   
   integer function make_directory_window(comm, addr) result(win)
      integer, intent(in) :: comm
      type (c_ptr), intent(out) :: addr

      type (Directory), pointer :: dir
      type (Directory), target  :: dirnull
      integer(kind=MPI_ADDRESS_KIND) :: sz
      integer :: ierror, rank

      call MPI_Comm_Rank(comm, rank, ierror)

      if (rank == 0)  then
         sz = sizeof_directory()
         call MPI_Alloc_mem(sz, MPI_INFO_NULL, addr, ierror)
         call c_f_pointer(addr, dir)
      else
         sz  = 0
         dir =>dirnull
      endif

      call MPI_Win_create(dir, sz, 1, MPI_INFO_NULL, comm, win, ierror)

   end function make_directory_window
   
   subroutine connect_to_server(this, port_name, client, client_comm, unusable, server_size, rc)
      use pFIO_ClientThreadMod
      class (DirectoryService), target, intent(inout) :: this
      character(*), intent(in) :: port_name
      class (ClientThread), intent(inout) :: client
      integer, intent(in) :: client_comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: server_size
      integer, optional, intent(out) :: rc

      class (AbstractSocket), pointer :: sckt
      integer :: rank_in_client
      integer :: ierror
      integer :: status(MPI_STATUS_SIZE)

      type (Directory) :: dir
      type (DirectoryEntry) :: dir_entry

      logical :: found
      integer :: n
      integer :: server_rank
      integer :: tmp_rank
      integer :: server_root_rank
      integer :: client_npes
      integer :: server_npes
      integer, allocatable :: client_ranks(:)
      integer, allocatable :: server_ranks(:)
      
      type(ServerThread), pointer :: server_thread_ptr
      class(BaseServer), pointer :: server_ptr

      ! First, check ports to see if server is local, in which case
      ! a SimpleSocket is used for the connection.
      ! Note: In this scenario, the server _must_ always publish prior to this.

      do n = 1, this%n_local_ports
         if (trim(this%local_ports(n)%port_name) == port_name) then
            allocate(sckt, source=SimpleSocket(client))
            server_ptr => this%local_ports(n)%server_ptr
            call server_ptr%add_connection(sckt)
            server_thread_ptr => server_ptr%threads%at(1) ! should be "last"
            allocate(sckt, source=SimpleSocket(server_thread_ptr))
            call client%set_connection(sckt)
            nullify(sckt)
            if (present(server_size)) server_size = server_ptr%npes
            allocate(server_ptr%serverthread_done_msgs(1))
            server_ptr%serverthread_done_msgs = .false.
            _RETURN(_SUCCESS)
         end if
      end do

      call MPI_Comm_rank(client_comm, rank_in_client, ierror)

      if (rank_in_client == 0) then

         call this%mutex%acquire()

         dir = this%get_directory(this%win_server_directory)

         found = .false.
         do n = 1, dir%num_entries
            if (port_name == dir%entries(n)%port_name) then
               if (dir%entries(n)%partner_root_rank >= 0) then
                  found = .true.
                  server_root_rank =  dir%entries(n)%partner_root_rank
               end if
               exit
            end if
         end do

         if (.not. found) then  ! advertise self

            dir = this%get_directory(this%win_client_directory)

            n = dir%num_entries + 1
            dir%num_entries = n

            dir_entry%port_name = port_name
            call MPI_Comm_rank(this%comm, dir_entry%partner_root_rank, ierror) ! global comm

            dir%entries(n) = dir_entry
         
            call this%put_directory(dir, this%win_client_directory)
         end if

         call this%mutex%release()

         if (found) then
            call MPI_Send(this%rank, 1, MPI_INTEGER, server_root_rank, DISCOVERY_TAG, this%comm, ierror)
         else
            call MPI_Recv(server_root_rank, 1, MPI_INTEGER, MPI_ANY_SOURCE, DISCOVERY_TAG, this%comm, status, ierror)
         end if

      end if

      ! complete handshake
      if (rank_in_client == 0) then
         call MPI_Comm_size(client_comm, client_npes, ierror)
         allocate(client_ranks(client_npes))
         allocate(server_ranks(client_npes))
      else
         allocate(client_ranks(1)) ! MPI does not like 0-sized arrays, even when they are unused
         allocate(server_ranks(1)) ! MPI does not like 0-sized arrays, even when they are unused
      end if

      call MPI_Gather(this%rank, 1, MPI_INTEGER, client_ranks, 1, MPI_INTEGER, 0, client_comm, ierror)
      if (rank_in_client == 0) then
         call MPI_Send(client_npes, 1, MPI_INTEGER, server_root_rank, NPES_TAG, this%comm, ierror)
         call MPI_Send(client_ranks, client_npes, MPI_INTEGER, server_root_rank, RANKS_TAG, this%comm, ierror)
         call MPI_Recv(server_ranks, client_npes, MPI_INTEGER, server_root_rank, 0, this%comm, status, ierror)
         call MPI_Recv(server_npes, 1, MPI_INTEGER, server_root_rank, 0, this%comm, status, ierror)
         if (present(server_size)) server_size = server_npes
      end if

      call MPI_Scatter(server_ranks, 1, MPI_INTEGER, &
        & server_rank, 1, MPI_INTEGER, &
        & 0, client_comm, ierror)
     
      if (present(server_size)) call MPI_Bcast(server_size, 1, MPI_INTEGER, 0, client_comm,ierror)

      ! Construct the connection
      call MPI_Recv(tmp_rank, 1, MPI_INTEGER, server_rank, CONNECT_TAG, this%comm, status, ierror)
      _ASSERT(tmp_rank == server_rank, "shake the wrong hand")

      allocate(sckt, source=MpiSocket(this%comm, server_rank, this%parser))
      call client%set_connection(sckt)
      _RETURN(_SUCCESS)
   end subroutine connect_to_server

   subroutine connect_to_client(this, port_name, server, rc)
      class (DirectoryService), target, intent(inout) :: this
      character(*), intent(in) :: port_name
      class (BaseServer), intent(inout) :: server
      integer, optional, intent(out) :: rc

      class (AbstractSocket), pointer :: sckt

      type (Directory) :: dir

      integer, allocatable :: counts(:), displs(:)
      integer :: client_rank
      integer :: client_root_rank
      integer, allocatable :: client_ranks(:)
      integer, allocatable :: my_client_ranks(:)
      integer, allocatable :: server_ranks(:)
      integer, allocatable :: my_server_ranks(:)
      integer :: status(MPI_STATUS_SIZE)

      integer :: p
      integer :: server_comm
      integer :: ierror
      logical :: found
      integer :: server_npes
      integer :: client_npes
      integer :: rank_in_server
      integer :: n
      integer :: cnts
      integer :: n_entries

      server%terminate  = .false.
      server_comm = MPI_COMM_NULL
      server_comm = server%get_communicator()
      if (server_comm == MPI_COMM_NULL) then
         _RETURN(_SUCCESS)
      endif

      call MPI_Comm_rank(server_comm, rank_in_server, ierror)

      if (rank_in_server == 0) then

         call this%mutex%acquire()

         dir = this%get_directory(this%win_client_directory)
         client_root_rank = -1

         found = .false.
         n_entries = dir%num_entries
         do n = 1, n_entries
            if (port_name == dir%entries(n)%port_name) then
               found = .true.
               client_root_rank =  dir%entries(n)%partner_root_rank
               exit
            end if
         end do

         if (found) then
            ! Clear entry
            dir%entries(n:n_entries-1) = dir%entries(n+1:n_entries)
            dir%entries(n_entries)%port_name = ''
            dir%entries(n_entries)%partner_root_rank = -1
            dir%num_entries = n_entries - 1
            call this%put_directory(dir, this%win_client_directory)
         end if

         call this%mutex%release()
      
         if (found) then
            call MPI_Send(this%rank, 1, MPI_INTEGER, client_root_rank, DISCOVERY_TAG, this%comm, ierror)
         else
            call MPI_Recv(client_root_rank, 1, MPI_INTEGER, MPI_ANY_SOURCE, DISCOVERY_TAG, this%comm, status, ierror)
         end if

         if (client_root_rank /= TERMINATE) then ! not a termination signal
            call MPI_Recv(client_npes, 1, MPI_INTEGER, client_root_rank, NPES_TAG, this%comm, status, ierror)
            allocate(client_ranks(client_npes))
            call MPI_Recv(client_ranks, client_npes, MPI_INTEGER, client_root_rank, RANKS_TAG, this%comm, status, ierror)
         else
            client_npes = TERMINATE
         end if

      end if


      call MPI_Comm_size(server_comm, server_npes, ierror)
      call MPI_Bcast(client_npes, 1, MPI_INTEGER, 0, server_comm, ierror)

      if (client_npes == TERMINATE) then
        server%terminate = .true.
        _RETURN(_SUCCESS)
      endif

      allocate(counts(0:server_npes-1), displs(0:server_npes-1))
      do p = 0, server_npes - 1
         counts(p) = ((p+1)*client_npes) / server_npes - (p*client_npes) / server_npes
      end do
      displs(0) = 0
      do p = 1, server_npes-1
         displs(p) = displs(p-1) + counts(p-1)
      end do

      cnts = counts(rank_in_server)
      allocate(my_client_ranks(cnts))

      allocate(server_ranks(client_npes))
      allocate(my_server_ranks(cnts))
      my_server_ranks = this%rank

      call MPI_GatherV(my_server_ranks, cnts, MPI_INTEGER, &
           & server_ranks, counts, displs, MPI_INTEGER, &
           & 0, server_comm, ierror)

      if (rank_in_server == 0) then
        call MPI_Send(server_ranks, client_npes, MPI_INTEGER, client_root_rank, 0, this%comm, ierror)
        call MPI_Send(server_npes,   1,          MPI_INTEGER, client_root_rank, 0, this%comm, ierror)
      endif

      if (rank_in_server /= 0) then
         allocate(client_ranks(1))
      end if
      call MPI_ScatterV(client_ranks, counts, displs, MPI_INTEGER, &
           & my_client_ranks, cnts, MPI_INTEGER, &
           & 0, server_comm, ierror)

      do p = 1, cnts
         client_rank = my_client_ranks(p)
         call MPI_Send(this%rank, 1, MPI_INTEGER, client_rank, CONNECT_TAG, this%comm, ierror)
         allocate(sckt, source=MpiSocket(this%comm, client_rank, this%parser))
         call server%add_connection(sckt)
         nullify(sckt)
      end do

      _RETURN(_SUCCESS)
   end subroutine connect_to_client

   ! This step is probably not actually needed at this time.
   ! But it would allow future implementation to query for servers
   ! or possibly to allow servers to satisfy multiple clients.
   subroutine publish(this, port, server, rc)
      class (DirectoryService), intent(inout) :: this
      type(PortInfo),target, intent(in) :: port
      class (BaseServer), intent(inout) :: server
      integer, optional, intent(out) :: rc
      character(len=MAX_LEN_PORT_NAME) :: port_name 
      integer :: ierror
      integer :: rank_in_server
      integer :: n
      

      type (Directory) :: dir
      type (DirectoryEntry) :: dir_entry
      logical :: found
      integer :: server_comm
      character(len=*), parameter :: Iam = __FILE__

      ! Update local ports
      this%n_local_ports = this%n_local_ports + 1
      this%local_ports(this%n_local_ports) = port
      server_comm = MPI_COMM_NULL
      server_comm = server%get_communicator()
      if (server_comm == MPI_COMM_NULL) then
         _RETURN(_SUCCESS)
      endif

      call MPI_Comm_rank(server_comm, rank_in_server, ierror)
      port_name = port%port_name

      if (rank_in_server == 0) then

         call this%mutex%acquire()

        ! Get - modify - put
         dir = this%get_directory(this%win_server_directory)

         ! Verify that server has not already published.
         found = .false.
         do n = 1, dir%num_entries
            if (port_name == dir%entries(n)%port_name) then
               if (dir%entries(n)%partner_root_rank >= 0) then
                  found = .true.
               end if
               exit
            end if
         end do

         _ASSERT(.not. found, 'not found port_name')

         n = dir%num_entries + 1
         dir%num_entries = n
         
         dir_entry%port_name = port_name
         dir_entry%partner_root_rank = this%rank
         dir%entries(n) = dir_entry
         call this%put_directory(dir, this%win_server_directory)

         call this%mutex%release()
      end if
      _RETURN(_SUCCESS)
   end subroutine publish


   function sizeof_directory() result(sz)
      integer :: sz
      
      integer :: sizeof_char, sizeof_integer, sizeof_DirectoryEntry
      integer :: one_integer
      character :: one_char

      sizeof_integer = c_sizeof(one_integer)
      sizeof_char    = c_sizeof(one_char)
      
      sizeof_DirectoryEntry = MAX_LEN_PORT_NAME*sizeof_char + 1*sizeof_integer
      sz = sizeof_integer + MAX_NUM_PORTS*sizeof_DirectoryEntry
   end function sizeof_directory


   integer function get_win(this, client_or_server) result(win)
      class (DirectoryService), intent(in) :: this
      character(len=*), intent(in) :: client_or_server

      select case (client_or_server)
      case ('server','server_directory')
         win = this%win_server_directory
      case ('client','client_directory')
         win = this%win_client_directory
      end select

   end function get_win


   function get_directory(this, win) result(dir)
      type (Directory) :: dir
      class (DirectoryService), intent(in) :: this
      integer, intent(in) :: win

      integer :: sz
      integer(kind=MPI_ADDRESS_KIND) :: disp
      integer :: ierror

      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, win, ierror)

      sz = sizeof_directory()
      disp = 0
      call MPI_Get(dir, sz, MPI_BYTE, 0, disp, sz, MPI_BYTE, win, ierror)

      call MPI_Win_unlock(0, win, ierror)
      return
      _UNUSED_DUMMY(this)
   end function get_directory
      

   subroutine put_directory(this, dir, win)
      class (DirectoryService), intent(in) :: this
      type (Directory), intent(in) :: dir
      integer, intent(in) :: win

      integer :: sz
      integer(kind=MPI_ADDRESS_KIND) :: disp
      integer :: ierror


      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, win, ierror)

      sz = sizeof_directory()
      disp = 0
      call MPI_put(dir, sz, MPI_BYTE, 0, disp, sz, MPI_BYTE, win, ierror)

      call MPI_Win_unlock(0, win, ierror)
      return
      _UNUSED_DUMMY(this)
   end subroutine put_directory
      
   subroutine terminate_servers(this, client_comm, rc)
      class (DirectoryService), intent(inout) :: this
      integer ,intent(in) :: client_comm
      integer, optional, intent(out) :: rc

      type (Directory) :: dir
      integer :: ierror, rank_in_client,i
      
      call MPI_Comm_rank(client_comm, rank_in_client, ierror)

      call MPI_BARRIER(client_comm,ierror)

      if (rank_in_client ==0) then
         
         write(6,*)"client0 terminates servers"; flush(6)

         dir = this%get_directory(this%win_server_directory)

         do i = 1, dir%num_entries

            call MPI_Send(TERMINATE, 1, MPI_INTEGER, dir%entries(i)%partner_root_rank, DISCOVERY_TAG, &
                & this%comm, ierror)

         enddo

      endif

      _RETURN(_SUCCESS)

   end subroutine terminate_servers

   subroutine free_directory_resources(this, rc)
      class (DirectoryService), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type (Directory), pointer :: dir
      integer :: ierror
      ! Release resources

      call MPI_Barrier(this%comm, ierror)

      call this%mutex%free_mpi_resources()

      call MPI_Win_free(this%win_server_directory, ierror)
      call MPI_Win_free(this%win_client_directory, ierror)

      if (this%rank == 0) then
         call c_f_pointer(this%server_dir, dir)
         call MPI_Free_mem(dir, ierror)
         call c_f_pointer(this%client_dir, dir)
         call MPI_Free_mem(dir, ierror)
      end if

      _RETURN(_SUCCESS)
   end subroutine free_directory_resources

end module pFIO_DirectoryServiceMod

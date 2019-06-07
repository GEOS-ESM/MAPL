#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.(A)) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
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
   use, intrinsic :: iso_c_binding, only: c_f_pointer, c_ptr
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractSocketMod
   use pFIO_MpiMutexMod
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
   public :: directory_service

   integer, parameter :: MAX_NUM_PORTS = 1
   integer, parameter :: TERMINATE = -1

   ! MPI Tags
   integer, parameter :: DISCOVERY_TAG = 1 ! Exchange of _root_ rank between client and server
   integer, parameter :: NPES_TAG = 2  ! Client sends number of pes in client to server  (on roots)
   integer, parameter :: RANKS_TAG = 3 ! Client sends ranks of client processes to server (on roots)
   integer, parameter :: CONNECT_TAG = 3 ! client and server individual processes exchange ranks 
   
   type :: DirectoryEntry
      sequence
      character(len=MAX_LEN_PORT_NAME) :: port_name
      integer :: rank
      integer :: npes
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
      type (ProtocolParser) :: parser
   contains
      procedure :: connect_to_server
      procedure :: connect_to_client
      procedure :: has_NewClient

      procedure :: publish
      procedure :: terminate_servers

      procedure :: get_win
      procedure :: get_directory
      procedure :: put_directory
   end type DirectoryService

   type (DirectoryService), target, save :: directory_service

   interface DirectoryService
      module procedure new_DirectoryService
   end interface DirectoryService

   integer, parameter :: MAX_PORTS = 1
   integer, parameter :: UNUSED_PORT = -1

contains

   function new_DirectoryService(comm, unusable, rc) result(ds)
      type (DirectoryService) :: ds
      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: ierror
      type (Directory) :: empty_dir

      character(len=*), parameter :: Iam = "DirectoryService::new_DirectoryService()"

      _UNUSED_DUMMY(unusable)

      call MPI_Comm_dup(comm, ds%comm, ierror)
      _VERIFY(ierror)
      call MPI_Comm_rank(ds%comm, ds%rank, ierror)
      _VERIFY(ierror)

      ! Create windows that will be used for coordination
      ! 1. lock - control modification of other windows
      ds%mutex = MpiMutex(ds%comm)
      ds%win_server_directory = make_directory_window(ds%comm)
      ds%win_client_directory = make_directory_window(ds%comm)

      if(ds%rank == 0) then
         call ds%put_directory(empty_dir, ds%win_client_directory)
         call ds%put_directory(empty_dir, ds%win_server_directory)
      end if

      ds%parser = ProtocolParser()
      ! Need to be sure that the directories have been initialized before
      ! proceeding
      call MPI_Barrier(comm, ierror)

   end function new_DirectoryService

   
   integer function make_directory_window(comm) result(win)
      integer, intent(in) :: comm
      
      type (Directory), pointer :: dir
      integer(kind=MPI_ADDRESS_KIND) :: sz
      type (c_ptr) :: addr
      integer :: ierror
      
      sz = sizeof_directory()
      
      call MPI_Alloc_mem(sz, MPI_INFO_NULL, addr, ierror)
      call c_f_pointer(addr, dir)
      call MPI_Win_create(dir, sz, 1, MPI_INFO_NULL, comm, win, ierror)

   end function make_directory_window
   
   function connect_to_server(this, port, comm) result(sckt)
      class (AbstractSocket), pointer :: sckt
      class (DirectoryService), target, intent(inout) :: this
      type(PortInfo),target,intent(in) :: port
      integer, intent(in) :: comm

      character(len=MAX_LEN_PORT_NAME) :: port_name
      integer :: rank_in_client
      integer :: ierror
      integer(kind=MPI_ADDRESS_KIND) :: disp
      integer :: status(MPI_STATUS_SIZE)

      type (Directory) :: dir
      type (DirectoryEntry) :: dir_entry

      logical :: found
      integer :: n
      integer :: server_rank
      integer :: server_root_rank
      integer :: client_npes
      integer, allocatable :: client_ranks(:)

      call MPI_Comm_rank(comm, rank_in_client, ierror)

      port_name = port%port_name

      if (rank_in_client == 0) then

         call this%mutex%acquire()

         dir = this%get_directory(this%win_server_directory)

         found = .false.
         do n = 1, dir%num_entries
            if (port_name == dir%entries(n)%port_name) then
               found = .true.
               server_root_rank =  dir%entries(n)%rank
               exit
            end if
         end do

         if (.not. found) then

            dir = this%get_directory(this%win_client_directory)

            n = dir%num_entries + 1
            dir%num_entries = n
            dir_entry%port_name = port_name

            call MPI_Comm_rank(this%comm, dir_entry%rank, ierror) ! global comm
            call MPI_Comm_size(comm, dir_entry%npes, ierror)
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
         call MPI_Comm_size(comm, client_npes, ierror)
         allocate(client_ranks(client_npes))
      else
         allocate(client_ranks(1)) ! MPI does not like 0-sized arrays, even when they are unused
      end if

      call MPI_Gather(this%rank, 1, MPI_INTEGER, client_ranks, 1, MPI_INTEGER, 0, comm, ierror)
      if (rank_in_client == 0) then
         call MPI_Send(client_npes, 1, MPI_INTEGER, server_root_rank, NPES_TAG, this%comm, ierror)
         call MPI_Send(client_ranks, client_npes, MPI_INTEGER, server_root_rank, RANKS_TAG, this%comm, ierror)
      end if

      ! Construct the connection
      call MPI_Recv(server_rank, 1, MPI_INTEGER, MPI_ANY_SOURCE, CONNECT_TAG, this%comm, status, ierror)

      allocate(sckt, source=MpiSocket(this%comm, server_rank, this%parser))
      
   end function connect_to_server

   subroutine connect_to_client(this, port, comm,sockets,shutdown)
      class (DirectoryService), target, intent(inout) :: this
      type(PortInfo),target,intent(in) :: port
      integer, intent(in) :: comm
      type(AbstractSocketVector),intent(inout) :: sockets
      logical, intent(out) :: shutdown
      character(len=MAX_LEN_PORT_NAME) :: port_name

      class (AbstractSocket), pointer :: sckt

      type (Directory) :: dir

      integer, allocatable :: counts(:), displs(:)
      integer :: client_rank
      integer :: client_root_rank
      integer, allocatable :: client_ranks(:)
      integer, allocatable :: my_client_ranks(:)
      integer :: status(MPI_STATUS_SIZE)

      integer :: p
      integer :: ierror
      logical :: found
      integer :: server_npes
      integer :: client_npes
      integer :: rank_in_server
      integer :: n
      integer :: cnts

      sockets= AbstractSocketVector() ! empty one
      shutdown  = .false.
      port_name = port%port_name
      ! look for new clients
      call MPI_Comm_rank(comm, rank_in_server, ierror)

      if (rank_in_server == 0) then

         call this%mutex%acquire()

         dir = this%get_directory(this%win_client_directory)

         found = .false.
         do n = 1, dir%num_entries
            if (port_name == dir%entries(n)%port_name) then
               found = .true.
               client_root_rank =  dir%entries(n)%rank
               exit
            end if
         end do
         
         if (found) then

            ! Clear entry
            dir%entries(n:dir%num_entries-1) = dir%entries(n+1:dir%num_entries)
            dir%num_entries = dir%num_entries - 1
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


      call MPI_Comm_size(comm, server_npes, ierror)
      call MPI_Bcast(client_npes, 1, MPI_INTEGER, 0, comm, ierror)

      if (client_npes == TERMINATE) then
        shutdown = .true.
        return
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

      if (rank_in_server /= 0) then
         allocate(client_ranks(1))
      end if
      call MPI_ScatterV(client_ranks, counts, displs, MPI_INTEGER, &
           & my_client_ranks, cnts, MPI_INTEGER, &
           & 0, comm, ierror)

      do p = 1, cnts
         client_rank = my_client_ranks(p)
         call MPI_Send(this%rank, 1, MPI_INTEGER, client_rank, CONNECT_TAG, this%comm, ierror)
         allocate(sckt, source=MpiSocket(this%comm, client_rank, this%parser))
         call sockets%push_back(sckt)
         nullify(sckt)
      end do

   end subroutine connect_to_client

   ! non-blacking check if there are new clients
   function has_NewClient(this, port_name, comm) result(has)
      logical :: has
      class (DirectoryService), target, intent(inout) :: this
      character(len=*), intent(in) :: port_name
      integer, intent(in) :: comm
      integer :: status(MPI_STATUS_SIZE)
      integer :: n,rank_in_server
      integer :: ierror
      type (Directory) :: dir
      logical :: found

      has = .false.
      call MPI_Comm_rank(comm, rank_in_server, ierror)

      if (rank_in_server == 0) then
         dir = this%get_directory(this%win_client_directory)
         found = .false.
         do n = 1, dir%num_entries
            if (port_name == dir%entries(n)%port_name) then
               found = .true.
               has = .true.
               exit
            end if
         end do
         if ( .not. found) then
            call MPI_iProbe(MPI_ANY_SOURCE,DISCOVERY_TAG, this%comm,has, status, ierror)
         endif
      end if

      call MPI_Bcast(has, 1, MPI_LOGICAL, 0, comm, ierror)

   end function has_NewClient
   
   subroutine publish(this, port, comm)
      class (DirectoryService), intent(inout) :: this
      type(PortInfo),target, intent(in) :: port
      integer, intent(in) :: comm
      character(len=MAX_LEN_PORT_NAME) :: port_name 
      integer :: ierror
      integer :: rank_in_server
      integer :: n

      type (Directory) :: dir
      type (DirectoryEntry) :: dir_entry

      call MPI_Comm_rank(comm, rank_in_server, ierror)
      port_name = port%port_name

      if (rank_in_server == 0) then
         call this%mutex%acquire()

         ! Get - modify - put
         dir = this%get_directory(this%win_server_directory)

         n = dir%num_entries + 1
         dir%num_entries = n

         dir_entry%port_name = port_name
         call MPI_Comm_rank(this%comm, dir_entry%rank, ierror) ! global rank
         call MPI_Comm_size(comm, dir_entry%npes, ierror)

         dir%entries(n) = dir_entry

         call this%put_directory(dir, this%win_server_directory)

         call this%mutex%release()
      end if

   end subroutine publish


   function sizeof_directory() result(sz)
      integer :: sz
      
      integer :: sizeof_char, sizeof_integer, sizeof_DirectoryEntry
      integer :: ierror
      
      call MPI_Type_extent(MPI_INTEGER, sizeof_integer, ierror)
      call MPI_Type_extent(MPI_CHARACTER, sizeof_char, ierror)

      sizeof_DirectoryEntry = MAX_LEN_PORT_NAME*sizeof_char + 2*sizeof_integer
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

   end subroutine put_directory
      
   subroutine terminate_servers(this,comm)
      class (DirectoryService), intent(inout) :: this
      integer ,intent(in) :: comm
      type (Directory) :: dir
      integer :: ierror, rank_in_client,i
      
      call MPI_Comm_rank(comm, rank_in_client, ierror)

      call MPI_BARRIER(comm,ierror)

      if (rank_in_client ==0) then
         
         print*,"client0 terminates servers"

         dir = this%get_directory(this%win_server_directory)

         do i = 1, dir%num_entries

            call MPI_Send(TERMINATE, 1, MPI_INTEGER, dir%entries(i)%rank, DISCOVERY_TAG, &
                & this%comm, ierror)

         enddo

      endif

   end subroutine terminate_servers

end module pFIO_DirectoryServiceMod

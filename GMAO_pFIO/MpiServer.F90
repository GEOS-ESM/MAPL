module pFIO_MpiServerMod

   use pFIO_AbstractDirectoryServiceMod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_AbstractServerMod
   use pFIO_IntegerIntegerMapMod
!   use pfio_base

   implicit none
   private

   public :: MpiServer

   type,extends (AbstractServer) :: MpiServer
   contains
      procedure :: start
   end type MpiServer


   interface MpiServer
      module procedure new_MpiServer
   end interface MpiServer


contains

   function new_MpiServer(comm, directory_service) result(s)
      type (MpiServer) :: s
      integer, intent(in) :: comm
      class (AbstractDirectoryService), target, optional :: directory_service

      call s%init(comm)
!      call pfio_init()

      if (present(Directory_Service)) then
         s%directory_service => directory_service
      else
         s%directory_service => chosen_directory_service
      end if

      call s%directory_service%publish(PortInfo('i_server'), s%comm)

   end function new_MpiServer

   subroutine start(this)
      class (MpiServer),target, intent(inout) :: this
      class (AbstractSocket), pointer :: sckt
      type (AbstractSocketVector) :: sockets
      type (ServerThreadVector) :: threads
      class (ServerThread),pointer :: threadPtr=>null()
      integer :: i,k,client_size
      logical, allocatable :: mask(:)

      ! disribute server at the beginnig, blocking
      threads = ServerThreadVector()

      ! initialize the server thread at the very begining
      call this%directory_service%connect_to_client(PortInfo('i_server'),this%comm,sockets,this%terminate)
      this%num_clients = this%num_clients+sockets%size()
      nullify(threadPtr)
      do k=1, sockets%size()
         sckt => sockets%at(k)
         allocate(threadPtr,source = ServerThread(sckt,this))
         call threads%push_back(threadPtr)
         nullify(threadPtr)
      enddo

      client_size = threads%size()
      allocate(mask(client_size))
      mask = .false.
      
      ! loop untill terminate
      do while (.true.)

         do i = 1,client_size

            if ( mask(i)) cycle

            threadPtr=>threads%at(i)
            !handle the message
            call threadPtr%run()
            !delete the thread object if it terminates 
            if(threadPtr%do_terminate()) then
               mask(i) = .true.
            endif
         enddo

         if (all(mask)) exit

      enddo

      call threads%clear()
      deallocate(mask)

      print*, "terminated successfully"

   end subroutine start

end module pFIO_MpiServerMod

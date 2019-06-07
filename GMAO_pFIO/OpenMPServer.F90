module pFIO_OpenMPServerMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_ServerThreadMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_AbstractServerMod
   use pFIO_IntegerIntegerMapMod

   implicit none
   private

   public :: OpenMPServer

   type,extends(AbstractServer) :: OpenMPServer
   contains
      procedure :: start
   end type OpenMPServer

   interface OpenMPServer
      module procedure new_OpenMPServer
   end interface OpenMPServer


contains

   function new_OpenMPServer(comm,directory_service) result(s)
      type (OpenMPServer) :: s
      integer, intent(in) :: comm
      class(AbstractDirectoryService), target, optional :: directory_service

      call s%init(comm)
     
      if (present(Directory_Service)) then
         s%directory_service => directory_service
      else
         s%directory_service => chosen_directory_service
      end if

      call s%directory_service%publish(PortInfo('i_server'), s%comm)

   end function new_OpenMPServer


   subroutine start(this)
      class (OpenMPServer),target, intent(inout) :: this
      class (AbstractSocket), pointer :: sckt
      type (AbstractSocketVector) :: sockets
      class (ServerThread),pointer :: threadPtr
      integer :: i,thread_size

      this%terminate = .false.
      sockets = AbstractSocketVector()

      !$omp parallel shared(this,sockets) 
      !$omp single

      call this%directory_service%connect_to_client(PortInfo('i_server'),this%comm,sockets,this%terminate)
      this%num_clients=this%num_clients+sockets%size()
          
      thread_size = sockets%size()

      do i =1,thread_size
         sckt => sockets%at(i)
         !$omp task private(threadPtr),firstprivate(sckt),shared(this)
         allocate(threadPtr,source = ServerThread(sckt,this))
         do while (.true.)
            call threadPtr%run()
            if(threadPtr%do_terminate()) then
               deallocate(threadPtr)
               exit
            endif
         enddo
         !$omp end task
       enddo

      !$omp taskwait
      !$omp end single
      !$omp end parallel
      print*, "terminated successfully"

   end subroutine start

end module pFIO_OpenMPServerMod

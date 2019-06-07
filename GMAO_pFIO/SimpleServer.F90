! This server is used for the case when clients and servers are on the same comm
module pFIO_SimpleServerMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_SimpleDirectoryServiceMod
   use pFIO_SimpleSocketMod
   use pFIO_ServerThreadMod
   use pFIO_AbstractServerMod
   use pFIO_AbstractMessageMod
   use pFIO_DoneMessageMod

   implicit none
   private

   public :: SimpleServer

   type,extends (AbstractServer) :: SimpleServer
     type(ServerThread) :: st
   contains
      procedure :: start
      procedure :: get_dmessage
   end type SimpleServer


   interface SimpleServer
      module procedure new_SimpleServer
   end interface SimpleServer


contains

   function new_SimpleServer(comm) result(s)
      type(SimpleServer),target :: s
      integer, intent(in) :: comm

      call s%init(comm)
      s%num_clients = 1

   end function new_SimpleServer

   subroutine start(this)
      class (SimpleServer),target, intent(inout) :: this
      type(SimpleSocket) :: empty
      
      type(PortInfo) :: ptinfo

      ptinfo%port_name='simple'
      ptinfo%partner_ptr => this%st
      empty = SimpleSocket()
      this%st = ServerThread(empty,this)  
 
      call chosen_directory_service%publish(ptinfo, this%comm)

   end subroutine start

   function get_dmessage(this) result(dmessage) 
      class (SimpleServer),target,intent(in) :: this
      class(AbstractMessage),pointer :: dmessage
      allocate(dmessage,source = DoneMessage())
   end function

end module pFIO_SimpleServerMod

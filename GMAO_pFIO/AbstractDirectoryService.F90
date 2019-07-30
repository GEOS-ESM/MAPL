module pFIO_AbstractDirectoryServiceMod
   use pFIO_BaseServerMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod

   implicit none
   private

   public :: AbstractDirectoryService  
   public :: MAX_LEN_PORT_NAME
   public :: MAX_NUM_PORTS
   public :: PortInfo

   integer,parameter :: MAX_NUM_PORTS = 2
   integer,parameter :: MAX_LEN_PORT_NAME= 16

   type :: PortInfo
     character(len=MAX_LEN_PORT_NAME) :: port_name
     ! it is for simple_directory_service, should be the type BaseThread pointer
     class(BaseServer), pointer :: server_ptr => null()
   end type

   type,abstract :: AbstractDirectoryService
   contains
      procedure(connect_to_server), deferred :: connect_to_server
      procedure(connect_to_client), deferred :: connect_to_client
      procedure(publish),deferred :: publish
   end type AbstractDirectoryService

   abstract interface

      subroutine connect_to_server(this, port_name, client, client_comm, rc)
         use pFIO_ClientThreadMod
         import AbstractDirectoryService
         import PortInfo
         import AbstractSocket
         class (AbstractDirectoryService), target, intent(inout) :: this
         character(len=*), intent(in) :: port_name
         class (ClientThread), intent(inout) :: client
         integer, intent(in) :: client_comm
         integer, optional, intent(out) :: rc
      end subroutine connect_to_server

      subroutine connect_to_client(this, port_name, server, rc)
         use pFIO_BaseServerMod
         import AbstractDirectoryService
         import PortInfo
         import AbstractSocketVector
         class (AbstractDirectoryService), target, intent(inout) :: this
         character(*), intent(in) :: port_name
         class (BaseServer), intent(inout) :: server
         integer, optional, intent(out) :: rc
      end subroutine connect_to_client

      subroutine publish(this, port, server, rc)
         use pFIO_BaseServerMod
         import AbstractDirectoryService
         import PortInfo
         class (AbstractDirectoryService), intent(inout) :: this
         type(PortInfo), target, intent(in) :: port
         class (BaseServer), intent(inout) :: server
         integer, optional, intent(out) :: rc
      end subroutine

   end interface

   interface PortInfo
      module procedure new_PortInfo
   endinterface

contains

   function new_PortInfo(port_name, server_ptr) result(port)
      character(*),intent(in) :: port_name
      class (BaseServer),target,optional,intent(in) :: server_ptr
      type(PortInfo) :: port
      port%port_name = port_name
      if(present(server_ptr)) port%server_ptr => server_ptr
   end function

end module pFIO_AbstractDirectoryServiceMod

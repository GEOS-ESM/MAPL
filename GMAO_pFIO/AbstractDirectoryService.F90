module pFIO_AbstractDirectoryServiceMod
   use pFIO_AbstractSocketMod
   use pFIO_MessageVisitorMod
   use pFIO_AbstractSocketVectorMod

   implicit none
   private

   public :: AbstractDirectoryService  
   public :: MAX_LEN_PORT_NAME
   public :: PortInfo
   public :: chosen_directory_service

   integer,parameter :: MAX_LEN_PORT_NAME= 16

   type :: PortInfo
     character(len=MAX_LEN_PORT_NAME) :: port_name
     ! it is for simple_directory_service, should be the type BaseThread pointer
     class(MessageVisitor),pointer :: partner_ptr=>null()
   end type

   type,abstract :: AbstractDirectoryService
   contains
      procedure(connect_to_server), deferred :: connect_to_server
      procedure(connect_to_client), deferred :: connect_to_client
      procedure(publish),deferred :: publish
   end type AbstractDirectoryService

   abstract interface

      function connect_to_server(this, port, comm) result(sckt)
         import AbstractDirectoryService
         import PortInfo
         import AbstractSocket
         class (AbstractSocket), pointer :: sckt
         class (AbstractDirectoryService), target, intent(inout) :: this
         type(PortInfo),target, intent(in) :: port
         integer, intent(in) :: comm
      end function connect_to_server

      subroutine connect_to_client(this, port, comm,sockets,shutdown)
         import AbstractDirectoryService
         import PortInfo
         import AbstractSocketVector
         class (AbstractDirectoryService), target, intent(inout) :: this
         type(PortInfo),target, intent(in) :: port
         integer, intent(in) :: comm
         type(AbstractSocketVector),intent(inout) :: sockets
         logical, intent(out) :: shutdown
      end subroutine connect_to_client

      subroutine publish(this, port, comm)
         import AbstractDirectoryService
         import PortInfo
         class (AbstractDirectoryService), intent(inout) :: this
         type(PortInfo), target, intent(in) :: port
         integer, intent(in) :: comm
      end subroutine

   end interface

   interface PortInfo
      module procedure new_PortInfo
   endinterface

   class(AbstractDirectoryService),pointer :: chosen_directory_service

contains

   function new_PortInfo(port_name, server_ptr) result(port)
      character(*),intent(in) :: port_name
      class(MessageVisitor),target,optional,intent(in) :: server_ptr
      type(PortInfo) :: port
      port%port_name = port_name
      if(present(server_ptr)) port%partner_ptr=>server_ptr
   end function

end module pFIO_AbstractDirectoryServiceMod

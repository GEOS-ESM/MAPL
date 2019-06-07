#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.(A)) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

module pFIO_SimpleDirectoryServiceMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractSocketMod
   use pFIO_SimpleSocketMod
   use pFIO_BaseThreadMod
   use pFIO_AbstractSocketVectorMod
   implicit none
   private

   public :: SimpleDirectoryService  
   public :: simple_directory_service

   type,extends(AbstractDirectoryService) :: SimpleDirectoryService
      private
      integer :: comm,rank
      class(BaseThread),pointer :: to_server=>null()
      class(BaseThread),pointer :: to_client=>null()
   contains

      procedure :: connect_to_server
      procedure :: connect_to_client
      procedure :: publish

   end type SimpleDirectoryService

   interface SimpleDirectoryService
      module procedure new_SimpleDirectoryService
   end interface SimpleDirectoryService

   type(SimpleDirectoryService),target :: simple_directory_service

contains

   function new_SimpleDirectoryService(comm, unusable, rc) result(sds)
      type (SimpleDirectoryService) :: sds
      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: ierror
      character(len=*), parameter :: Iam = "SimpleDirectoryService::"

      _UNUSED_DUMMY(unusable)

      call MPI_Comm_dup(comm, sds%comm, ierror)
      _VERIFY(ierror)
      call MPI_Comm_rank(sds%comm, sds%rank, ierror)
      _VERIFY(ierror)

   end function 

   subroutine publish(this, port, comm)
      class (SimpleDirectoryService), intent(inout) :: this
      type(PortInfo),target,intent(in) :: port
      integer, intent(in) :: comm

      select type (thread=>port%partner_ptr)
      class is (BaseThread)
        this%to_server=>thread
      class default
        stop " inappropriate class"
      end select

   end subroutine

   function connect_to_server(this, port, comm) result(sckt)
      class (AbstractSocket), pointer :: sckt
      class (SimpleDirectoryService), target, intent(inout) :: this
      type(PortInfo),target,intent(in) :: port
      integer, intent(in) :: comm
      type (SimpleSocket) :: client_sckt
      type (SimpleSocket) :: server_sckt

      if( .not. associated(this%to_server)) stop "publish server first"

      select type (thread=>port%partner_ptr)
      class is (BaseThread)
        this%to_client=>thread
      class default
        stop "inappropriate client class"
      end select

      client_sckt=SimpleSocket(this%to_server)
      server_sckt=SimpleSocket(this%to_client)

      call this%to_server%set_connection(server_sckt)

      allocate(sckt,source=client_sckt)

   end function connect_to_server

   subroutine connect_to_client(this, port, comm,sockets,shutdown)
      class (SimpleDirectoryService), target, intent(inout) :: this
      type(PortInfo), target, intent(in) :: port
      integer, intent(in) :: comm
      type(AbstractSocketVector),intent(inout) :: sockets
      logical, intent(out) :: shutdown
   end subroutine connect_to_client

end module pFIO_SimpleDirectoryServiceMod

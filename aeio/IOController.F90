#include "MAPL_Generic.h"

module AEIO_IOController
   use MAPL_ExceptionHandling
   use ESMF
   use AEIO_Client
   use AEIO_ClientMap
   use AEIO_Server
   use AEIO_ServerMap
   use yafYaml
   use HistoryConfigMod
   use CollectionMod
   use CollectionRegistryMod
   use gFTL_StringVector
   
   implicit none
   private

   ! functions of IO controller
   ! - instantiate server/front+back and clients on appropriate pets
   ! - serve as common interface to generate route handle on full VM
   ! - take state from application and deliver to client
   ! - take config and pass down to server and client
   ! - control execution of client and servers and creating epochs

   ! - in theory if not want separate server resources could instatiate server on same nodes
   ! - and skip the epochs have to think about how RH work then ..., if RH calling 
   ! - delegated to client/server 
   type IOController
      private
      type(ClientMap) :: clients
      type(ServerMap) :: servers

   contains
      procedure :: initialize
   end type

contains

   ! state
   ! resource distribution
   ! configuration file or yaml object if already parsed
   subroutine initialize(this,state,configuration_file,clock,pet_list,rc)
      class(IOController), intent(inout) :: this
      type(ESMF_State), intent(in ) :: state
      character(len=*), intent(inout) :: configuration_file
      integer, intent(in) :: pet_list(:,:)
      type(ESMF_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      type(HistoryConfig) :: hist_config
      type(CollectionRegistry) :: coll_registry
      type(StringVector ):: enabled
      type(StringVectorIterator) :: enabled_iter
      type(Collection) :: hist_coll
      character(:), allocatable :: key
      type(Server) :: output_server
      type(Client) :: output_client

      integer :: status

      call hist_config%import_yaml_file(configuration_file,rc=status)
      _VERIFY(status)

      ! initialize collections on front of server
      enabled = hist_config%get_enabled()
      coll_registry=hist_config%get_collections()
      enabled_iter = enabled%begin()
      do while(enabled_iter /= enabled%end())
         key=enabled_iter%get()
         hist_coll=coll_registry%at(key)
         output_server=Server(hist_coll,pet_list,rc=status)
         _VERIFY(status)
         call this%servers%insert(key,output_server)
         output_client=Client(hist_coll,pet_list,rc=status)
         _VERIFY(status)
         call this%clients%insert(key,output_client)
      enddo
      
      _RETURN(_SUCCESS)
   end subroutine initialize


end module AEIO_IOController

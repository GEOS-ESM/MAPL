#include "MAPL.h"
module pFIO_ClientManagerMod

   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use pFIO_ClientThreadMod
   use pFIO_FastClientThreadMod
   use pFIO_StringClientThreadMapMod

   implicit none
   private

   public :: add_client
   public :: get_client

   type(StringClientThreadMap), target, private :: client_map

contains

   subroutine add_client(name, client, rc)
      character(*), intent(in) :: name
      class(ClientThread), intent(in) :: client
      integer, optional, intent(out) :: rc

      call client_map%insert(name, client)
      _RETURN(_SUCCESS)
   end subroutine add_client

   function get_client(name, rc) result(client)
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc
      class(ClientThread), pointer :: client

      client => client_map%at(name)
      _ASSERT(associated(client), "Client '"//name//"' not found in client manager")
      _RETURN(_SUCCESS)
   end function get_client

end module pFIO_ClientManagerMod

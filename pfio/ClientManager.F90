#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ClientManagerMod

   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use pFIO_ClientThreadMod
   use pFIO_FastClientThreadMod
   use pFIO_StringClientThreadMapMod

   implicit none
   private

   public :: init_IO_ClientManager
   public :: get_client_thread
   public :: client_map

   interface init_IO_ClientManager
      module procedure init_ClientManager
   end interface

   type(StringClientThreadMap), target, protected :: client_map

contains

   subroutine init_ClientManager(client_comm, unusable, fast_oclient, rc)
      integer, intent(in) :: client_comm
      class (KeywordEnforcer), optional, intent(out) :: unusable
      logical, optional, intent(in) :: fast_oclient
      integer, optional, intent(out) :: rc
      integer :: status

      logical :: fast_
      type(ClientThread) :: i_client
      class(ClientThread), allocatable :: o_client

      fast_ = .false.
      if (present(fast_oclient)) fast_ = fast_oclient

      client_map = StringClientThreadMap()

      i_client = ClientThread(client_comm=client_comm, rc=status)
      _VERIFY(status)
      call client_map%insert('i_client', i_client)

      if (fast_) then
         allocate(o_client, source=FastClientThread(client_comm=client_comm, rc=status))
      else
         allocate(o_client, source=ClientThread(client_comm=client_comm, rc=status))
      end if
      _VERIFY(status)
      call client_map%insert('o_client', o_client)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine init_ClientManager

   function get_client_thread(name, rc) result(client)
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc
      class(ClientThread), pointer :: client

      client => client_map%at(name)
      _ASSERT(associated(client), "Client '"//name//"' not found in client manager")
      _RETURN(_SUCCESS)
   end function get_client_thread

end module pFIO_ClientManagerMod

#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ClientManagerMod

   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use pFIO_ClientThreadMod
   use pFIO_FastClientThreadMod

   implicit none
   private

   public :: init_IO_ClientManager
   public :: i_Client
   public :: o_Client

   interface init_IO_ClientManager
      module procedure init_ClientManager
   end interface

   class(ClientThread), allocatable, target :: i_Client
   class(ClientThread), allocatable, target :: o_Client

contains

   subroutine init_ClientManager(client_comm, unusable, fast_oclient, rc)
      integer, intent(in) :: client_comm
      class (KeywordEnforcer), optional, intent(out) :: unusable
      logical, optional, intent(in) :: fast_oclient
      integer, optional, intent(out) :: rc
      integer :: status

      logical :: fast_

      fast_ = .false.
      if (present(fast_oclient)) fast_ = fast_oclient

      if (allocated(i_Client)) deallocate(i_Client)
      allocate(i_Client, source=ClientThread(client_comm=client_comm, rc=status))
      _VERIFY(status)

      if (allocated(o_Client)) deallocate(o_Client)
      if (fast_) then
         allocate(o_Client, source=FastClientThread(client_comm=client_comm, rc=status))
      else
         allocate(o_Client, source=ClientThread(client_comm=client_comm, rc=status))
      end if
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine init_ClientManager

end module pFIO_ClientManagerMod

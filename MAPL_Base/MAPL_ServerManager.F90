module MAPL_ServerManagerMod

   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use PFIO

   implicit none
   private

   public :: i_ClientPtr
   public :: o_ClientPtr
   public :: init_servers

   type(ClientThread), pointer :: i_ClientPtr
   type(ClientThread), pointer :: o_ClientPtr

   contains

   subroutine init_servers()
      allocate(i_ClientPtr, source = ClientThread())
      allocate(o_ClientPtr, source = ClientThread())
   end subroutine

end module MAPL_ServerManagerMod

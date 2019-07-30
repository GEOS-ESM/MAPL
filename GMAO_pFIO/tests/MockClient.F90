module MockClientMod
   use pFIO_ClientThreadMod
   implicit none
   private

   public :: MockClient

   type, extends(ClientThread) :: MockClient
   end type MockClient

   interface MockClient
      module procedure new_MockClient
   end interface MockClient

contains

   function new_MockClient() result(c)
      type (MockClient) :: c
   end function new_MockClient
   
   
end module MockClientMod

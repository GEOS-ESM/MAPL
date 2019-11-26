#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_ioClientsMod

   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use pFIO_ClientManagerMod
   use PFIO

   implicit none
   private

   public :: io_client
   public :: i_clients
   public :: o_clients
   type(ClientManager), target :: i_Clients
   type(ClientManager), target :: o_Clients

   type, public :: IO_Clients
      type(IntegerVector) :: o_clients_size
      type(IntegerVector) :: i_clients_size
      type(IntegerVector) :: large_oclient_pool
      type(IntegerVector) :: small_oclient_pool
      integer :: smallCurrent=1
      integer :: largeCurrent=1
      integer :: writeCutoff
      contains
         procedure :: init_io_clients
         procedure :: set_size
         procedure :: split_oclient_pool
         procedure :: set_oclient
   end type

   type(IO_Clients), target :: io_client

contains

   subroutine init_io_clients(this, unusable, ni, no, rc)
      class (IO_Clients), intent(inout) :: this
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(in) :: ni, no
      integer, optional, intent(out) :: rc
      integer :: n_i, n_o
      n_i = 1
      n_o = 1
      if (present(ni)) n_i = ni
      if (present(no)) n_o = no

      i_Clients = ClientManager(n_client=n_i)
      o_Clients = ClientManager(n_client=n_o)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine

   subroutine set_size(this, unusable, ni, no, rc)
      class (IO_Clients), intent(inout) :: this
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(in) :: ni(:), no(:)
      integer, optional, intent(out) :: rc
      integer :: i
      if (present(no)) then
         _ASSERT(size(no)==o_clients%size(),"mismatch in sizes")
         do i=1,size(no)
            call this%o_clients_size%push_back(no(i))
         enddo
      end if 
      if (present(ni)) then
         _ASSERT(size(ni)==i_Clients%size(),"mismatch in sizes")
         do i=1,size(ni)
            call this%i_Clients_size%push_back(ni(i))
         enddo
      end if 

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine

   subroutine split_oclient_pool(this, nsplit, nwriteCutoff,unusable, rc)
      class (IO_Clients), intent(inout) :: this
      integer, intent(in) :: nsplit
      integer, intent(in) :: nWriteCutoff
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i

      do i=1,this%o_clients_size%size()
          if (this%o_clients_size%at(i) >= nsplit) then 
             call this%large_oclient_pool%push_back(i)
          else
             call this%small_oclient_pool%push_back(i)
          end if
      enddo
      this%writeCutoff = nwriteCutoff
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine

   subroutine set_oclient(this,nwriting,unusable,rc)
      class (IO_Clients), intent(inout) :: this
      integer, intent(in) :: nwriting
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

       
      ! no "small" pool, just get next
      if (this%small_oclient_pool%size() == 0) then
         call o_Clients%next()
      else
         _ASSERT(this%large_oclient_pool%size()>0,'large server pool must be great than zero')
         _ASSERT(this%small_oclient_pool%size()>0,'small server pool must be great than zero')
         if (nwriting .ge. this%writeCutoff) then
            this%largeCurrent=this%largeCurrent+1
            if (this%largeCurrent .gt. this%large_oclient_pool%size()) this%largeCurrent=1
            call o_clients%set_current(this%large_oclient_pool%at(this%largeCurrent))
         else
            this%smallCurrent=this%smallCurrent+1
            if (this%smallCurrent .gt. this%small_oclient_pool%size()) this%smallCurrent=1
            call o_clients%set_current(this%small_oclient_pool%at(this%smallCurrent))
         end if     
      end if
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine

end module MAPL_ioClientsMod

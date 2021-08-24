#include "MAPL_Generic.h"


module AEIO_Client
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   
   implicit none
   private

   public :: Client
   ! functions of IO controller
   ! - instantiate Client/front+back and Clients on appropriate pets
   ! - serve as common interface to generate route handle on full VM
   ! - take state from application and deliver to Client
   ! - take config and pass down to Client and Client
   ! - control execution of Client and Clients and creating epochs

   ! - in theory if not want separate Client resources could instatiate Client on same nodes
   ! - and skip the epochs have to think about how RH work then ..., if RH calling 
   ! - delegated to Client/Client 
   type Client
      type(collection) :: hist_collection
      integer, allocatable :: pet_list(:,:)
      type(ESMF_FieldBundle) :: bundle
   contains
      procedure initialize
   end type

   interface Client
      module procedure new_Client
   end interface CLient

contains

   ! state
   ! resource distribution
   ! configuration file or yaml object if already parsed

   subroutine initialize(this,state,rc)
      class(Client), intent(inout) :: this
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      

      _RETURN(_SUCCESS)
   end subroutine initialize

   function new_Client(hist_collection,pet_list,rc) result(c)
      type(Collection), intent(in) :: hist_collection 
      integer, intent(in)          :: pet_list(:,:)
      integer, optional, intent(out) :: rc
      type(Client) :: c
      integer :: status

      c%hist_collection = hist_collection
      allocate(c%pet_list,source=pet_list,stat=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end function new_Client


end module AEIO_Client

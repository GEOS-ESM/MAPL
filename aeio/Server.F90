#include "MAPL_Generic.h"


module AEIO_Server
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   
   implicit none
   private

   public :: Server
   ! functions of IO controller
   ! - instantiate Server/front+back and Servers on appropriate pets
   ! - serve as common interface to generate route handle on full VM
   ! - take state from application and deliver to Server
   ! - take config and pass down to Server and Server
   ! - control execution of Server and Servers and creating epochs

   ! - in theory if not want separate Server resources could instatiate Server on same nodes
   ! - and skip the epochs have to think about how RH work then ..., if RH calling 
   ! - delegated to Server/Server 
   type Server
      type(collection) :: hist_collection
      integer, allocatable :: pet_list(:,:)
   contains
      procedure initialize
   end type

   interface Server
      module procedure new_Server
   end interface Server

contains

   ! state
   ! resource distribution
   ! configuration file or yaml object if already parsed

   subroutine initialize(this)
      class(Server), intent(inout) :: this
   end subroutine initialize

   function new_Server(hist_collection,pet_list,rc) result(c)
      type(Collection), intent(in) :: hist_collection 
      integer, intent(in)          :: pet_list(:,:)
      integer, optional, intent(out) :: rc
      type(Server) :: c
      integer :: status

      c%hist_collection = hist_collection
      allocate(c%pet_list,source=pet_list,stat=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end function new_Server


end module AEIO_Server

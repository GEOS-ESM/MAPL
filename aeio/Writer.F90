#include "MAPL_Generic.h"


module AEIO_Server
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   use AEIO_RHConnectorMap
   
   implicit none
   private

   public :: Writer

   type Writer
      logical :: I_am_writers_root
      integer, allocatable :: ranks(:)
      integer, allocatable :: server_ranks(:)
      type(RHConnectorMap) :: connectors
   contains
      procedure :: start_writers
   end type

   interface Writer
      module procedure new_Writer
   end interface Writer

contains

   function new_Server(hist_collection,pet_list,rc) result(c)
      type(Collection), intent(in) :: hist_collection 
      integer, intent(in)          :: pet_list(:,:)
      integer, optional, intent(out) :: rc
      type(Server) :: c
      integer :: status

   end function new_Server

   subroutine start_writer(this,rc)
      class(Writer), intent(inout) :: this
      intent, optional, intent(out) :: rc

   end subroutine writers

end module AEIO_Server

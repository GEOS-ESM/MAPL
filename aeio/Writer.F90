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
      integer, allocatable :: writer_ranks(:)
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
      integer :: status,myPet
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm,__RC__)
      call ESMF_VMGet(vm,localPet=myPet,__RC__)
      allocate (c%server_ranks,source=pet_list(2,:),stat=status)
      _VERIFY(status)
      allocate (c%writer_ranks,source=pet_list(3,:),stat=status)
      _VERIFY(status)
      c%i_am_writers_root = ( myPet == c%writer_ranks(1) )

   end function new_Server

   subroutine start_writer(this,rc)
      class(Writer), intent(inout) :: this
      intent, optional, intent(out) :: rc
      integer, parameter :: stag = 6782

      if (this%i_am_back_root()) then
         do while (.true.) then
             

         enddo
      else
         do while (.true.) then

         enddo
      end if

   end subroutine writers

end module AEIO_Server

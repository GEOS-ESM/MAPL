#include "MAPL_Generic.h"


module AEIO_Server
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   
   implicit none
   private

   public :: Server

   type Server
      integer, allocatable :: pet_list(:,:)
      type(ESMF_FieldBundle) :: bundle
   contains
      procedure initialize
      procedure set_grid
      procedure get_grid
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

      _RETURN(_SUCCESS)

   end function new_Server

   subroutine set_grid(this,grid,rc)
      class(server), intent(inout) :: this
      type(esmf_grid), intent(in) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      
      call ESMF_FieldBundleSet(this%bundle,grid,__RC__)
      _RETURN(_SUCCESS)

   end subroutine set_grid

   function get_grid(this,rc) result(grid)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(esmf_grid) :: grid
      integer :: status
      
      call ESMF_FieldBundleGet(this%bundle,grid=grid,__RC__)
      _RETURN(_SUCCESS)

   end function get_grid

end module AEIO_Server

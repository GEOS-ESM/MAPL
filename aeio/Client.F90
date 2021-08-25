#include "MAPL_Generic.h"


module AEIO_Client
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   
   implicit none
   private

   public :: Client
   type Client
      type(collection) :: hist_collection
      integer, allocatable :: pet_list(:,:)
      type(ESMF_FieldBundle) :: bundle
   contains
      procedure initialize
      procedure get_grid
   end type

   interface Client
      module procedure new_Client
   end interface Client

contains

   subroutine initialize(this,state,rc)
      class(Client), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: collection_name

      collection_name = this%hist_collection%get_name()
      this%bundle=ESMF_FieldBundleCreate(name=collection_name,rc=status)
      _VERIFY(status)
      call this%hist_collection%fill_bundle(state,this%bundle,rc=status)
      _VERIFY(status) 

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

   function get_grid(this,rc) result(grid)
      class(Client), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      call ESMF_FieldBundleGet(this%bundle,grid=grid,rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function get_grid

end module AEIO_Client

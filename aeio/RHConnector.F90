#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
module AEIO_RHConnector
   use ESMF
   use MAPL_ExceptionHandling

   implicit none
   private

   public RHConnector

   type :: RHConnector
      private
      type(ESMF_RouteHandle) :: rh
      logical :: sender
      type(ESMF_StateItem_Flag) :: rh_container_type
      logical :: regrid
   contains
      procedure regrid_store_FieldBundles
      procedure regrid_store_fields
      !procedure redist_store_ArrayBundles
      !procedure redist_store_arrays
      procedure set_sender
      procedure regrid_FieldBundles
      procedure regrid_fields
      !procedure redist_FieldBundles
      !procedure redist_Fields
      !procedure redist_ArrayBundles
      !procedure redist_Arrays
   end type

contains

   subroutine set_sender(this,sender)
      class(RHConnector), intent(inout) :: this
      logical, intent(in) :: sender
      this%sender=sender
   end subroutine set_sender

   subroutine set_type(this,esmf_type)
      class(RHConnector), intent(inout) :: this
      type(ESMF_StateItem_Flag), intent(In) :: esmf_type
      this%rh_container_type = esmf_type
   end subroutine set_type
    
   subroutine regrid_store_FieldBundles(this,FieldBundle_in,FieldBundle_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: FieldBundle_in
      type(ESMF_FieldBundle), intent(out) :: FieldBundle_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldBundleRegridStore(FieldBundle_in,FieldBundle_out,routeHandle=this%rh,rc=status)
      _VERIFY(status)
      this%rh_container_type=ESMF_STATEITEM_FIELDBUNDLE
      this%regrid=.true.
      _RETURN(_SUCCESS)
   end subroutine regrid_store_FieldBundles

   subroutine regrid_FieldBundles(this,FieldBundle_in,FieldBundle_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: FieldBundle_in
      type(ESMF_FieldBundle), intent(out) :: FieldBundle_out
      integer, optional, intent(out) :: rc

      if (this%sender) then
         call ESMF_FieldBundleRegrid(srcFieldBundle=FieldBundle_in,routeHandle=this%rh)
      else
         call ESMF_FieldBundleRegrid(dstFieldBundle=FieldBundle_out,routeHandle=this%rh)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine regrid_FieldBundles
    
   subroutine regrid_store_fields(this,Field_in,Field_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Field), intent(in) :: Field_in
      type(ESMF_Field), intent(out) :: Field_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldRegridStore(Field_in,Field_out,routeHandle=this%rh,rc=status)
      _VERIFY(status)
      this%rh_container_type=ESMF_STATEITEM_FIELDBUNDLE
      this%regrid=.true.
      _RETURN(_SUCCESS)
   end subroutine regrid_store_fields

   subroutine regrid_fields(this,Field_in,Field_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Field), intent(in) :: Field_in
      type(ESMF_Field), intent(out) :: Field_out
      integer, optional, intent(out) :: rc

      if (this%sender) then
         call ESMF_FieldRegrid(srcField=Field_in,routeHandle=this%rh)
      else
         call ESMF_FieldRegrid(dstField=Field_out,routeHandle=this%rh)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine regrid_fields
    
end module AEIO_RHConnector

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
   contains
      procedure setup_from_FieldBundles
      procedure setup_from_fields
      procedure set_sender
      procedure regrid_FieldBundles
      procedure regrid_fields
   end type

contains

   subroutine set_sender(this,sender)
      class(RHConnector), intent(inout) :: this
      logical, intent(in) :: sender
      this%sender=sender
   end subroutine set_sender
    
   subroutine setup_from_FieldBundles(this,FieldBundle_in,FieldBundle_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: FieldBundle_in
      type(ESMF_FieldBundle), intent(out) :: FieldBundle_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldBundleRegridStore(FieldBundle_in,FieldBundle_out,routeHandle=this%rh,rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine setup_from_FieldBundles

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
    
   subroutine setup_from_fields(this,Field_in,Field_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Field), intent(in) :: Field_in
      type(ESMF_Field), intent(out) :: Field_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldRegridStore(Field_in,Field_out,routeHandle=this%rh,rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine setup_from_fields

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

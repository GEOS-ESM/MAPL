#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
module AEIO_RHConnector
   use ESMF
   use MAPL_ExceptionHandling

   implicit none
   !private

   public RHConnector

   type :: RHConnector
      !private
      type(ESMF_RouteHandle) :: rh
      logical :: sender
      type(ESMF_StateItem_Flag) :: rh_container_type
      logical :: regrid
   contains
      procedure regrid_store_FieldBundles
      procedure regrid_store_fields
      procedure redist_store_FieldBundles
      procedure redist_store_fields
      procedure redist_store_ArrayBundles
      procedure redist_store_arrays
      procedure set_sender
      procedure transfer_rh
      procedure regrid_FieldBundles
      procedure regrid_fields
      procedure redist_FieldBundles
      procedure redist_fields
      procedure redist_ArrayBundles
      procedure redist_Arrays
      procedure destroy
   end type

contains

   function transfer_rh(this,originPetList,targetPetList,rc) result(new_rh)
      class(RHConnector), intent(in) :: this
      integer, intent(in) :: originPetList(:)
      integer, intent(in) :: targetPetList(:)
      integer, intent(out), optional :: rc

      type(RHConnector) :: new_rh
      integer :: status

      new_rh%sender = this%sender
      new_rh%rh_container_type = this%rh_container_type
      new_rh%regrid=this%regrid
      new_rh%rh = ESMF_RouteHandleCreate(this%rh,originPetList=originPetList, &
                  targetPetList=targetPetList,_RC)
      _RETURN(_SUCCESS)
   end function transfer_rh

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
      type(ESMF_FieldBundle), intent(inout) :: FieldBundle_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldBundleRegridStore(FieldBundle_in,FieldBundle_out,routeHandle=this%rh,_RC)
      this%rh_container_type=ESMF_STATEITEM_FIELDBUNDLE
      this%regrid=.true.
      _RETURN(_SUCCESS)
   end subroutine regrid_store_FieldBundles

   subroutine regrid_store_fields(this,Field_in,Field_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Field), intent(in) :: Field_in
      type(ESMF_Field), intent(inout) :: Field_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldRegridStore(Field_in,Field_out,routeHandle=this%rh,_RC)
      this%rh_container_type=ESMF_STATEITEM_FIELD
      this%regrid=.true.
      _RETURN(_SUCCESS)
   end subroutine regrid_store_fields

   subroutine redist_store_FieldBundles(this,FieldBundle_in,FieldBundle_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: FieldBundle_in
      type(ESMF_FieldBundle), intent(inout) :: FieldBundle_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldBundleRedistStore(FieldBundle_in,FieldBundle_out,routeHandle=this%rh,_RC)
      this%rh_container_type=ESMF_STATEITEM_FIELDBUNDLE
      this%regrid=.false.
      _RETURN(_SUCCESS)
   end subroutine redist_store_FieldBundles

   subroutine redist_store_fields(this,Field_in,Field_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Field), intent(in) :: Field_in
      type(ESMF_Field), intent(inout) :: Field_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_FieldredistStore(Field_in,Field_out,routeHandle=this%rh,_RC)
      this%rh_container_type=ESMF_STATEITEM_FIELD
      this%regrid=.false.
      _RETURN(_SUCCESS)
   end subroutine redist_store_fields

   subroutine redist_store_arrayBundles(this,arrayBundle_in,arrayBundle_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_arrayBundle), intent(in) :: arrayBundle_in
      type(ESMF_arrayBundle), intent(inout) :: arrayBundle_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_ArrayBundleRedistStore(arrayBundle_in,arrayBundle_out,routeHandle=this%rh,_RC)
      this%rh_container_type=ESMF_STATEITEM_ARRAYBUNDLE
      this%regrid=.true.
      _RETURN(_SUCCESS)
   end subroutine redist_store_arrayBundles

   subroutine redist_store_arrays(this,array_in,array_out,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_array), intent(in) :: array_in
      type(ESMF_array), intent(inout) :: array_out
      integer, optional, intent(out) :: rc
      integer :: status
      call ESMF_ArrayRedistStore(array_in,array_out,routeHandle=this%rh,_RC)
      this%rh_container_type=ESMF_STATEITEM_ARRAY
      this%regrid=.true.
      _RETURN(_SUCCESS)
   end subroutine redist_store_arrays

   subroutine regrid_FieldBundles(this,srcFieldBundle,dstFieldBundle,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in), optional :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout) , optional :: dstFieldBundle
      integer, optional, intent(out) :: rc
      integer :: status

      if (this%sender) then
         call ESMF_FieldBundleRegrid(srcFieldBundle=srcFieldBundle,routeHandle=this%rh,_RC)
      else
         call ESMF_FieldBundleRegrid(dstFieldBundle=dstFieldBundle,routeHandle=this%rh,_RC)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine regrid_FieldBundles

   subroutine regrid_fields(this,srcField,dstField,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Field), intent(in), optional :: srcField
      type(ESMF_Field), intent(inout), optional :: dstField
      integer, optional, intent(out) :: rc
      integer :: status

      if (this%sender) then
         call ESMF_FieldRegrid(srcField=srcField,routeHandle=this%rh,_RC)
      else
         call ESMF_FieldRegrid(dstField=dstField,routeHandle=this%rh,_RC)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine regrid_fields
    
   subroutine redist_FieldBundles(this,srcFieldBundle,dstFieldBundle,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in), optional :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout), optional :: dstFieldBundle
      integer, optional, intent(out) :: rc
      integer :: status

      if (this%sender) then
         call ESMF_FieldBundleRegrid(srcFieldBundle=srcFieldBundle,routeHandle=this%rh,_RC)
      else
         call ESMF_FieldBundleRedist(dstFieldBundle=dstFieldBundle,routeHandle=this%rh,_RC)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine redist_FieldBundles

   subroutine redist_fields(this,srcField,dstField,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Field), intent(in), optional :: srcField
      type(ESMF_Field), intent(inout), optional :: dstField
      integer, optional, intent(out) :: rc
      integer :: status

      if (this%sender) then
         call ESMF_Fieldredist(srcField=srcField,routeHandle=this%rh,_RC)
      else
         call ESMF_Fieldredist(dstField=dstField,routeHandle=this%rh,_RC)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine redist_fields
    
   subroutine redist_ArrayBundles(this,srcArrayBundle,dstArrayBundle,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_ArrayBundle), intent(in), optional :: srcArrayBundle
      type(ESMF_ArrayBundle), intent(inout), optional :: dstArrayBundle
      integer, optional, intent(out) :: rc
      integer :: status

      if (this%sender) then
         call ESMF_ArrayBundleRedist(srcArrayBundle=srcArrayBundle,routeHandle=this%rh,_RC)
      else
         call ESMF_ArrayBundleRedist(dstArrayBundle=dstArrayBundle,routeHandle=this%rh,_RC)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine redist_ArrayBundles

   subroutine redist_Arrays(this,srcArray,dstArray,rc)
      class(RHConnector), intent(inout) :: this
      type(ESMF_Array), intent(in), optional :: srcArray
      type(ESMF_Array), intent(inout), optional :: dstArray
      integer, optional, intent(out) :: rc
      integer :: status

      if (this%sender) then
         call ESMF_ArrayRedist(srcArray=srcArray,routeHandle=this%rh,_RC)
      else
         call ESMF_ArrayRedist(dstArray=dstArray,routeHandle=this%rh,_RC)
      end if
      _RETURN(_SUCCESS)
     
   end subroutine redist_Arrays

   subroutine destroy(this,rc)
      class(RHConnector), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_RouteHandleDestroy(this%rh,noGarbage=.true.,_RC)
      _RETURN(_SUCCESS)
   end subroutine destroy
    
end module AEIO_RHConnector

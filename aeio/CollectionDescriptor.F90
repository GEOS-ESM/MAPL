#include "MAPL_Generic.h"


module AEIO_CollectionDescriptor
   use ESMF
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   
   implicit none
   private

   public :: CollectionDescriptor

   type CollectionDescriptor
      character(len=:), allocatable :: coll_name
      type(ESMF_FieldBundle) :: bundle
      type(RHConnector) :: rh
   contains
      procedure get_bundle
      procedure get_rh
      procedure get_coll_name    
   end type CollectionDescriptor 

   interface collectionDescriptor
      module procedure new_collectionDescriptor
   end interface collectionDescriptor

contains

   function new_CollectionDescriptor(bundle,collection_name,rh) result(collection_descriptor)
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: collection_name
      type(RHConnector), intent(in) :: rh
      type(collectionDescriptor) :: collection_descriptor
      collection_descriptor%coll_name=collection_name
      collection_descriptor%bundle=bundle
      collection_descriptor%rh=rh
   end function new_CollectionDescriptor
 
   function get_rh(this) result(rh)
      class(CollectionDescriptor), intent(inout) :: this
      type(RHConnector) :: rh
      rh = this%rh
   end function get_rh

   function get_bundle(this) result(bundle)
      class(CollectionDescriptor), intent(inout) :: this
      type(ESMF_FieldBundle) :: bundle
      bundle = this%bundle
   end function get_bundle

   function get_coll_name(this) result(coll_name)
      class(CollectionDescriptor), intent(inout) :: this
      character(len=:), allocatable :: coll_name
      coll_name = this%coll_name
   end function get_coll_name

end module AEIO_CollectionDescriptor

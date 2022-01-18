#include "MAPL_Generic.h"


module AEIO_CollectionDescriptor
   use ESMF
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   use pFIO
   use MAPL_BaseMod
   
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
      procedure write_bundle_from_array 
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
      call collection_descriptor%rh%set_sender(.false.)
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

   subroutine write_bundle_from_array(this,arrays,output_file,rc)
      class(CollectionDescriptor), intent(inout) :: this
      type(ESMF_ArrayBundle), intent(inout) :: arrays
      character(len=*), intent(in) :: output_file
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: vdims
      integer :: fieldCount
      integer :: gdims(3),i,rank,lb(1),ub(1)
      type(ESMF_Grid) :: grid
      type(ESMF_Array) :: array
      type(ESMF_Field) :: field
      type(FileMetaData) :: metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable) :: v 
      character(len=ESMF_MAXSTR), allocatable :: fieldNames(:)
      real, pointer :: ptr2d(:,:),ptr3d(:,:,:)
      logical :: have_lev
      integer :: lev_size

      call ESMF_FieldBundleGet(this%bundle,fieldCount=fieldCount,grid=grid,_RC)
      allocate(fieldNames(fieldCount))
      call ESMF_FieldBundleGet(this%bundle,fieldNameList=fieldNames,_RC)
      have_lev = .false.
      do i=1,fieldCount
         call ESMF_ArrayBundleGet(arrays,trim(fieldNames(i)),array=array,_RC)
         call ESMF_ArrayGet(array,rank=rank,_RC)
         if (rank==3) then
           call ESMF_ArrayGet(array,farrayPtr=ptr3d,_RC)
           have_lev = .true.
           lev_size = size(ptr3d,3)
         end if
      enddo

      call MAPL_GridGet(grid,globalCellCountPerDim=gdims,_RC)
      call metadata%add_dimension('Xdim',gdims(1),_RC)
      call metadata%add_dimension('Ydim',gdims(2),_RC)
      call metadata%add_dimension('lev',lev_size,_RC)
      call metadata%add_dimension('time',1,_RC)
      do i=1,fieldCount
         call ESMF_FieldBundleGet(this%bundle,trim(fieldNames(i)),field=field,_RC)
         call ESMF_FieldGet(field,rank=rank,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
         if (rank ==2) then
            vdims ="Xdim,Ydim,time"
         else if (rank==3) then
            vdims ="Xdim,Ydim,lev,time"
         end if
         v = Variable(type=PFIO_REAL32,dimensions=vdims,_RC)
         call metadata%add_variable(trim(fieldNames(i)),v,_RC)
      enddo
      call formatter%create(output_file,_RC)
      call formatter%write(metadata,_RC)
      do i=1,fieldCount
         call ESMF_ArrayBundleGet(arrays,trim(fieldNames(i)),array=array,_RC)
         call ESMF_ArrayGet(array,rank=rank,undistLBound=lb,undistUBound=ub,_RC)
         if (rank ==2) then
           call ESMF_ArrayGet(array,farrayPtr=ptr2d,_RC)
           call formatter%put_var(trim(fieldNames(i)),ptr2d,_RC)
         else if (rank==3) then
           call ESMF_ArrayGet(array,farrayPtr=ptr3d,_RC)
           call formatter%put_var(trim(fieldNames(i)),ptr3d,_RC)
         end if
      enddo
      call formatter%close()
      _RETURN(_SUCCESS)

   end subroutine write_bundle_from_array

end module AEIO_CollectionDescriptor

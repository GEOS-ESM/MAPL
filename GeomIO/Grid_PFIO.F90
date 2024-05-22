#include "MAPL_Generic.h"

module mapl3g_GridPFIO
   use mapl_ErrorHandling
   use mapl3g_GeomPFIO
   use mapl3g_SharedIO
   use ESMF
   use PFIO
   use MAPL_BaseMod
   use MAPL_FieldPointerUtilities
   use mapl3g_pFIOServerBounds
   use, intrinsic :: iso_c_binding, only: c_ptr
   implicit none
   private

   public :: GridPFIO
   type, extends (GeomPFIO) :: GridPFIO
      private
   contains
      procedure :: stage_data_to_file
   end type GridPFIO


contains

   subroutine stage_data_to_file(this, bundle, filename, time_index, rc)
      class(GridPFIO), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: filename
      integer, intent(in) :: time_index
      integer, intent(out), optional :: rc

      integer :: status, num_fields, i, collection_id
      character(len=ESMF_MAXSTR), allocatable :: field_names(:)
      type(ESMF_Field) :: field
      type(ArrayReference) :: ref
      integer, allocatable :: local_start(:), global_start(:), global_count(:)
      type(c_ptr) :: address
      integer :: type_kind
      type(ESMF_TypeKind_Flag) :: tk
      integer, allocatable :: element_count(:), new_element_count(:) 

      type(ESMF_Grid) :: grid
      type(pFIOServerBounds) :: server_bounds

      collection_id = this%get_collection_id()
      call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, _RC)
      allocate(field_names(num_fields))
      call ESMF_FieldBundleGet(bundle, fieldNameList=field_names, _RC)
      do i=1,num_fields
         call ESMF_FieldBundleGet(bundle, field_names(i), field=field, _RC)

         element_count = FieldGetLocalElementCount(field, _RC)
         call ESMF_FieldGet(field, grid=grid, typekind=tk,  _RC)

         call server_bounds%initialize(grid, element_count, time_index=time_index, _RC)
         global_start = server_bounds%get_global_start()
         global_count = server_bounds%get_global_count()
         local_start = server_bounds%get_local_start()
         
         ! generate array reference
         call FieldGetCptr(field, address, _RC)
         type_kind = esmf_to_pfio_type(tk, _RC)
         new_element_count = server_bounds%get_file_shape()
         ref = ArrayReference(address, type_kind, new_element_count)

         call o_clients%collective_stage_data(collection_id,filename, trim(field_names(i)), &
              ref, start=local_start, global_start=global_start, global_count=global_count)
      enddo

      _RETURN(_SUCCESS)

   end subroutine stage_data_to_file

end module mapl3g_GridPFIO

#include "MAPL_Generic.h"

module mapl3g_GridPFIO
   use mapl_ErrorHandling
   use mapl3g_GeomPFIO
   use ESMF
   use PFIO
   use MAPL_BaseMod
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
      integer, intent(in), optional :: time_index
      integer, intent(out), optional :: rc

      integer :: status, num_fields, i, collection_id
      character(len=ESMF_MAXSTR), allocatable :: field_names(:)
      type(ESMF_Field) :: field
      type(ArrayReference) :: ref
      real, pointer :: ptr2d(:,:)
      integer, allocatable :: local_start(:), global_start(:), global_count(:)

      type(ESMF_Grid) :: grid
      integer :: global_dim(3), i1, j1, in, jn

      collection_id = this%get_collection_id()
      call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, _RC)
      allocate(field_names(num_fields))
      call ESMF_FieldBundleGet(bundle, fieldNameList=field_names, _RC)
      do i=1,num_fields
         call ESMF_FieldBundleGet(bundle, field_names(i), field=field, _RC)
         ! all this logic needs to be generalized
         call ESMF_FieldGet(field, farrayPtr=ptr2d, _RC)
         allocate(global_start, source=[1,1,time_index])
         call ESMF_FieldGet(field, grid=grid, _RC)
         call MAPL_GridGet(grid, globalCellCountPerDim=global_dim, _RC)
         allocate(global_count, source=[global_dim(1),global_dim(2),1])
         call MAPL_GridGetInterior(grid, i1, in, j1, jn)
         allocate(local_start, source=[i1, j1,1])
         ref = ArrayReference(ptr2d)
         ! end generalization
         call o_clients%collective_stage_data(collection_id,filename, trim(field_names(i)), &
              ref, start=local_start, global_start=global_start, global_count=global_count)
      enddo

      _RETURN(_SUCCESS)

   end subroutine stage_data_to_file

end module mapl3g_GridPFIO

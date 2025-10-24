#include "MAPL.h"

module mapl3g_GridPFIO

   use, intrinsic :: iso_c_binding, only: c_ptr

   use mapl_ErrorHandling
   use mapl3g_GeomPFIO
   use mapl3g_SharedIO
   use ESMF
   use PFIO
   use MAPL_BaseMod
   use MAPL_FieldPointerUtilities
   use mapl3g_pFIOServerBounds, only: pFIOServerBounds, PFIO_BOUNDS_WRITE, PFIO_BOUNDS_READ
   use, intrinsic :: iso_c_binding, only: c_loc

   implicit none
   private

   public :: GridPFIO
   type, extends (GeomPFIO) :: GridPFIO
      private
      real(ESMF_KIND_R8), allocatable :: lons(:,:), lats(:,:), corner_lons(:,:), corner_lats(:,:)
   contains
      procedure :: stage_data_to_file
      procedure :: request_data_from_file
      procedure :: stage_coordinates_to_file
   end type GridPFIO

contains

   subroutine stage_coordinates_to_file(this, filename, rc)
      class(GridPFIO), intent(inout) :: this
      character(len=*), intent(in) :: filename
      integer, intent(out), optional :: rc

      integer :: status, collection_id
      logical :: has_ll
      type(FileMetadata) :: file_metadata
      type(ESMF_Grid) :: grid
      type(ESMF_Geom) :: EsmfGeom
      type(ESMF_Field) :: field
      integer, allocatable :: local_start(:), global_start(:), global_count(:)
      integer, allocatable :: element_count(:)
      type(pFIOServerBounds) :: server_bounds
      type(ESMF_TypeKind_Flag) :: tk
      type(c_ptr) :: address
      type(ArrayReference) :: ref
      real(ESMF_Kind_R8), pointer :: coords(:,:)

      file_metadata = this%get_file_metadata()
      has_ll = file_metadata%has_variable('lons') .and. file_metadata%has_variable('lats') 
      if (has_ll) then
         collection_id = this%get_collection_id()
         EsmfGeom = this%get_esmf_geom()
         call ESMF_GeomGet(EsmfGeom, grid=grid, _RC)
         call ESMF_GridGet(grid, coordTypeKind=tk, _RC)
         field = ESMF_FieldCreate(grid=grid, typekind=tk, _RC)
         element_count = FieldGetLocalElementCount(field, _RC)
         server_bounds = pFIOServerBounds(grid, element_count, PFIO_BOUNDS_WRITE, _RC)
         global_start = server_bounds%get_global_start()
         global_count = server_bounds%get_global_count()
         local_start = server_bounds%get_local_start()

         call ESMF_GridGetCoord(grid, 1, farrayPtr=coords, _RC)
         if (allocated(this%lons)) deallocate(this%lons)
         allocate(this%lons(size(coords,1), size(coords,2)), _STAT)
         this%lons = coords*MAPL_RADIANS_TO_DEGREES
         ref = ArrayReference(this%lons)
         call o_clients%collective_stage_data(collection_id,filename, 'lons', &
              ref, start=local_start, global_start=global_start, global_count=global_count)

         call ESMF_GridGetCoord(grid, 2, farrayPtr=coords, _RC)
         if (allocated(this%lats)) deallocate(this%lats)
         allocate(this%lats(size(coords,1), size(coords,2)), _STAT)
         this%lats = coords*MAPL_RADIANS_TO_DEGREES
         ref = ArrayReference(this%lats)
         call o_clients%collective_stage_data(collection_id,filename, 'lats', &
              ref, start=local_start, global_start=global_start, global_count=global_count)

         call ESMF_FieldDestroy(field, noGarbage=.true., _RC)
      end if

      has_ll = file_metadata%has_variable('corner_lons') .and. file_metadata%has_variable('corner_lats') 
      if (has_ll) then
         collection_id = this%get_collection_id()
         EsmfGeom = this%get_esmf_geom()
         call ESMF_GeomGet(EsmfGeom, grid=grid, _RC)
         call ESMF_GridGet(grid, coordTypeKind=tk, _RC)
         field = ESMF_FieldCreate(grid=grid, typekind=tk, staggerLoc=ESMF_STAGGERLOC_CORNER, _RC)
         element_count = FieldGetLocalElementCount(field, _RC)
         server_bounds = pFIOServerBounds(grid, element_count, PFIO_BOUNDS_WRITE, _RC)
         global_start = server_bounds%get_corner_global_start()
         global_count = server_bounds%get_corner_global_count()
         local_start = server_bounds%get_corner_local_start()

         call ESMF_GridGetCoord(grid, 1, farrayPtr=coords, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)
         if (allocated(this%corner_lats)) deallocate(this%corner_lats)
         allocate(this%corner_lons(size(coords,1), size(coords,2)), _STAT)
         this%corner_lons = coords*MAPL_RADIANS_TO_DEGREES
         ref = ArrayReference(this%corner_lons)
         call o_clients%collective_stage_data(collection_id,filename, 'corner_lons', &
              ref, start=local_start, global_start=global_start, global_count=global_count)

         call ESMF_GridGetCoord(grid, 2, farrayPtr=coords, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)
         if (allocated(this%corner_lats)) deallocate(this%corner_lats)
         allocate(this%corner_lats(size(coords,1), size(coords,2)), _STAT)
         this%corner_lats = coords*MAPL_RADIANS_TO_DEGREES
         ref = ArrayReference(this%corner_lats)
         call o_clients%collective_stage_data(collection_id,filename, 'corner_lats', &
              ref, start=local_start, global_start=global_start, global_count=global_count)

         call ESMF_FieldDestroy(field, noGarbage=.true., _RC)
      end if
      _RETURN(_SUCCESS)
   end subroutine stage_coordinates_to_file

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

         server_bounds = pFIOServerBounds(grid, element_count, PFIO_BOUNDS_WRITE, time_index=time_index, _RC)
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

   subroutine request_data_from_file(this, filename, bundle, rc)
      ! Arguments
      class(GridPFIO), intent(inout) :: this
      character(len=*), intent(in) :: filename
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, intent(out), optional :: rc

      character(len=ESMF_MAXSTR), allocatable :: field_names(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      type(ESMF_Grid) :: grid
      type(ESMF_TypeKind_Flag) :: esmf_typekind
      type(pFIOServerBounds) :: server_bounds
      integer, allocatable :: element_count(:), new_element_count(:)
      integer, allocatable :: local_start(:), global_start(:), global_count(:)
      type(c_ptr) :: address
      type(ArrayReference) :: ref
      integer :: collection_id, num_fields, idx, pfio_typekind, status

      collection_id = this%get_collection_id()

      call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, _RC)
      allocate(field_names(num_fields), _STAT)
      call ESMF_FieldBundleGet(bundle, fieldNameList=field_names, _RC)
      do idx = 1, num_fields
         call ESMF_FieldBundleGet(bundle, fieldName=field_names(idx), field=field, _RC)
         call ESMF_FieldGet(field, grid=grid, status=field_status, typekind=esmf_typekind, _RC)
         _ASSERT(field_status == ESMF_FIELDSTATUS_COMPLETE, "ESMF field is not complete")
         element_count = FieldGetLocalElementCount(field, _RC)
         server_bounds = pFIOServerBounds(grid, element_count, PFIO_BOUNDS_READ, _RC)
         global_start = server_bounds%get_global_start()
         global_count = server_bounds%get_global_count()
         local_start = server_bounds%get_local_start()
         call FieldGetCptr(field, address, _RC)
         pfio_typekind = esmf_to_pfio_type(esmf_typekind, _RC)
         new_element_count = server_bounds%get_file_shape()
         ref = ArrayReference(address, pfio_typekind, new_element_count)
         call i_Clients%collective_prefetch_data( &
              collection_id, &
              filename, &
              field_names(idx), &
              ref, &
              start=local_start, &
              global_start=global_start, &
              global_count=global_count)
      end do

      _RETURN(_SUCCESS)
   end subroutine request_data_from_file

end module mapl3g_GridPFIO

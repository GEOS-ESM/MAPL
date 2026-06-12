#include "MAPL.h"
module mapl_ExtDataReader_mod
   use esmf
   use gftl2_StringStringMap
   use gftl2_StringIntegerMap
   use MAPL
   use pFlogger, only: logger
   use, intrinsic :: iso_c_binding, only: c_ptr
   implicit none(type,external)
   private

   public :: ExtDataReader

   type ExtDataReader
      type(ESMF_FieldBundle) :: accumulated_fields
      type(StringStringMap) :: alias_map
      type(StringStringMap) :: filename_map
      type(StringIntegerMap) :: time_index_map
      type(StringIntegerMap) :: client_id_map
      contains
         procedure :: add_item
         procedure :: read_items
         procedure :: initialize_reader
         procedure :: destroy_reader
   end type ExtDataReader

   contains

   subroutine initialize_reader(this, rc)
      class(ExtDataReader), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%accumulated_fields = MAPL_FieldBundleCreate(name="reader_bundle", _RC)

      _RETURN(_SUCCESS)
   end subroutine initialize_reader

   subroutine destroy_reader(this, rc)
      class(ExtDataReader), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleDestroy(this%accumulated_fields, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy_reader

   subroutine add_item(this, field, alias, filename, time_index, client_id, rc)
      class(ExtDataReader), intent(inout) :: this
      type(ESMF_Field), intent(in) :: field
      character(len=*), intent(in) :: alias
      character(len=*), intent(in) :: filename
      integer, intent(in) :: time_index
      integer, intent(in) :: client_id
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR) :: field_name
      integer :: status

      call ESMF_FieldGet(field, name=field_name, _RC)
      call this%alias_map%insert(trim(field_name), alias)
      call this%filename_map%insert(trim(field_name), filename)
      call this%time_index_map%insert(trim(field_name), time_index)
      call this%client_id_map%insert(trim(field_name), client_id)
      call ESMF_FieldBundleAdd(this%accumulated_fields, [field], _RC)

      _RETURN(_SUCCESS)

   end subroutine add_item

   subroutine read_items(this, lgr, rc)
      class(ExtDataReader), intent(inout) :: this
      class(logger), pointer :: lgr
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR) :: field_name
      integer, pointer :: client_id, time_index
      character(len=:), pointer :: alias, filename
      integer :: status, i, pfio_typekind, num_fields
      type(ESMF_Field), allocatable :: field_list(:)
      type(ESMF_Grid) :: grid
      type(ESMF_TypeKind_Flag) :: esmf_typekind
      integer, allocatable :: element_count(:), new_element_count(:)
      integer, allocatable :: local_start(:), global_start(:), global_count(:)
      type(mapl_pFIOServerBounds) :: server_bounds
      type(c_ptr) :: address
      type(mapl_ArrayReference) :: ref

      call ESMF_FieldBundleGet(this%accumulated_fields, fieldCount=num_fields, _RC)
      if (num_fields == 0) then
         _RETURN(_SUCCESS)
      end if

      call MAPL_FieldBundleGet(this%accumulated_fields, fieldList=field_list, _RC)
      do i=1,size(field_list)
         call ESMF_FieldGet(field_list(i), name=field_name, _RC)
         alias => this%alias_map%at(trim(field_name))
         filename => this%filename_map%at(trim(field_name))
         client_id => this%client_id_map%at(trim(field_name))
         time_index => this%time_index_map%at(trim(field_name))
         call ESMF_FieldGet(field_list(i), grid=grid, typekind=esmf_typekind, _RC)
         element_count = MAPL_FieldGetLocalElementCount(field_list(i), _RC)


         server_bounds = mapl_pFIOServerBounds(grid, element_count, MAPL_PFIO_BOUNDS_READ, time_index=time_index, _RC)


         global_start = server_bounds%get_global_start()
         global_count = server_bounds%get_global_count()
         local_start = server_bounds%get_local_start()
         call MAPL_FieldGetCptr(field_list(i), address, _RC)

         pfio_typekind = mapl_esmf_to_pfio_type(esmf_typekind, _RC)
         new_element_count = server_bounds%get_file_shape()
         ref = mapl_ArrayReference(address, pfio_typekind, new_element_count)
         call mapl_i_Clients%collective_prefetch_data( &
              client_id, &
              filename, &
              alias, &
              ref, &
              start=local_start, &
              global_start=global_start, &
              global_count=global_count)
         deallocate(global_start, global_count, local_start, element_count, new_element_count)
         call lgr%info('reading %a from file %a at time index %i0.5', alias, filename, time_index)
      enddo
      call mapl_i_Clients%done_collective_prefetch()
      call mapl_i_Clients%wait()

      _RETURN(_SUCCESS)
   end subroutine

end module mapl_ExtDataReader_mod

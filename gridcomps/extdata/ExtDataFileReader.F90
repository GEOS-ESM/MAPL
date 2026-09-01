#include "MAPL.h"
module mapl_ExtDataReader_mod
   use esmf
    use gftl2_StringStringMap
    use gftl2_StringIntegerMap
    use gFTL2_StringSet
    use MAPL
    use mapl_DefaultServerNames_mod, only: MAPL_DEFAULT_INPUT_SERVER
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
        type(StringIntegerMap) :: prefetch_only_map
        character(:), allocatable :: input_server_name
       contains
         procedure :: add_item
         procedure :: read_items
         procedure :: initialize_reader
         procedure :: destroy_reader
         procedure :: get_unique_filenames
   end type ExtDataReader

   contains

    subroutine initialize_reader(this, input_server_name, rc)
       class(ExtDataReader), intent(inout) :: this
       character(len=*), intent(in), optional :: input_server_name
       integer, optional, intent(out) :: rc

       integer :: status

       this%accumulated_fields = MAPL_FieldBundleCreate(name="reader_bundle", _RC)
       this%input_server_name = MAPL_DEFAULT_INPUT_SERVER
       if (present(input_server_name)) this%input_server_name = input_server_name

      _RETURN(_SUCCESS)
   end subroutine initialize_reader

   subroutine destroy_reader(this, rc)
      class(ExtDataReader), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleDestroy(this%accumulated_fields, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy_reader

    subroutine add_item(this, field, alias, filename, time_index, client_id, prefetch_only, rc)
       class(ExtDataReader), intent(inout) :: this
       type(ESMF_Field), intent(in) :: field
       character(len=*), intent(in) :: alias
       character(len=*), intent(in) :: filename
       integer, intent(in) :: time_index
       integer, intent(in) :: client_id
       logical, intent(in), optional :: prefetch_only
       integer, optional, intent(out) :: rc

       character(len=ESMF_MAXSTR) :: field_name
       integer :: status, prefetch_only_value

      call ESMF_FieldGet(field, name=field_name, _RC)
       call this%alias_map%insert(trim(field_name), alias)
       call this%filename_map%insert(trim(field_name), filename)
       call this%time_index_map%insert(trim(field_name), time_index)
       call this%client_id_map%insert(trim(field_name), client_id)
       prefetch_only_value = 0
       if (present(prefetch_only)) then
          if (prefetch_only) prefetch_only_value = 1
       end if
       call this%prefetch_only_map%insert(trim(field_name), prefetch_only_value)
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
      integer :: status, i, pass, pfio_typekind, num_fields, request_id
      class(ClientThread), pointer :: i_client
      type(ESMF_Field), allocatable :: field_list(:)
      type(ESMF_Grid) :: grid
      type(ESMF_TypeKind_Flag) :: esmf_typekind
      integer, allocatable :: element_count(:), new_element_count(:)
      integer, allocatable :: local_start(:), global_start(:), global_count(:)
      type(mapl_pFIOServerBounds) :: server_bounds
      type(c_ptr) :: address
      type(mapl_ArrayReference) :: ref
      integer, pointer :: prefetch_only

      call ESMF_FieldBundleGet(this%accumulated_fields, fieldCount=num_fields, _RC)
      if (num_fields == 0) then
         _RETURN(_SUCCESS)
      end if

      i_client => mapl_get_client(this%input_server_name, _RC)

      call MAPL_FieldBundleGet(this%accumulated_fields, fieldList=field_list, _RC)
      do pass = 0, 1
         do i=1,size(field_list)
            call ESMF_FieldGet(field_list(i), name=field_name, _RC)
            alias => this%alias_map%at(trim(field_name))
            filename => this%filename_map%at(trim(field_name))
            client_id => this%client_id_map%at(trim(field_name))
            prefetch_only => this%prefetch_only_map%at(trim(field_name))
            if (prefetch_only /= pass) cycle
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
            if (prefetch_only == 0) then
               request_id = i_client%collective_prefetch_data( &
                   client_id, &
                   filename, &
                   alias, &
                   ref, &
                   start=local_start, &
                   global_start=global_start, &
                   global_count=global_count)
               call lgr%info('reading %a from file %a at time index %i0.5', alias, filename, time_index)
            else
               request_id = i_client%collective_prefetch_data_cache_only( &
                   client_id, &
                   filename, &
                   alias, &
                   ref, &
                   start=local_start, &
                   global_start=global_start, &
                   global_count=global_count)
               call lgr%info('prefetching next %a from file %a at time index %i0.5', alias, filename, time_index)
            end if
            deallocate(global_start, global_count, local_start, element_count, new_element_count)
         end do
      end do
      call i_client%done_collective_prefetch()
      call i_client%wait_all()

       _RETURN(_SUCCESS)
    end subroutine read_items

   subroutine get_unique_filenames(this, fileset, rc)
      class(ExtDataReader), intent(in), target :: this
      type(StringSet), intent(inout) :: fileset
      integer, optional, intent(out) :: rc

      type(StringStringMapIterator) :: iter
      character(len=:), pointer :: filename
      integer :: status

      iter = this%filename_map%ftn_begin()
      do while (iter /= this%filename_map%ftn_end())
         call iter%next()
         filename => iter%second()
         call fileset%insert(filename)
      end do

      _RETURN(_SUCCESS)
   end subroutine get_unique_filenames

end module mapl_ExtDataReader_mod

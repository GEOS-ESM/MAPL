#include "MAPL_Generic.h"

module mapl3g_Restart

   use, intrinsic :: iso_c_binding, only: c_ptr
   use esmf
   use mapl3g_geom_mgr, only: MaplGeom, get_geom_manager
   use mapl3g_MultiState, only: MultiState
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use mapl3g_geomio, only: bundle_to_metadata, GeomPFIO, make_geom_pfio, get_mapl_geom
   use mapl3g_pFIOServerBounds, only: pFIOServerBounds
   use mapl3g_SharedIO, only: esmf_to_pfio_type
   use MAPL_FieldPointerUtilities, only: FieldGetCPtr, FieldGetLocalElementCount
   use pFIO, only: PFIO_READ, FileMetaData, NetCDF4_FileFormatter
   use pFIO, only: i_Clients, o_Clients, ArrayReference

   implicit none
   private

   public :: Restart

   type :: Restart
      private
      character(len=ESMF_MAXSTR) :: gc_name
      type(ESMF_Geom) :: gc_geom
      type(ESMF_Time) :: current_time
   contains
      procedure, public :: write
      procedure, public :: read
      procedure, private :: write_bundle_
   end type Restart

   interface Restart
      procedure, private :: initialize_
   end interface Restart

contains

   function initialize_(gc_name, gc_geom, gc_clock, rc) result(new_restart)
      character(len=*), intent(in) :: gc_name
      type(ESMF_Geom), intent(in) :: gc_geom
      type(ESMF_Clock), intent(in) :: gc_clock
      integer, optional, intent(out) :: rc
      type(Restart) :: new_restart ! result

      integer :: status

      new_restart%gc_name = ESMF_UtilStringLowerCase(trim(gc_name), _RC)
      call ESMF_Clockget(gc_clock, currTime = new_restart%current_time, _RC)
      new_restart%gc_geom = gc_geom

      _RETURN(ESMF_SUCCESS)
   end function initialize_

   subroutine write(this, state_type, state, rc)
      ! Arguments
      class(Restart), intent(inout) :: this
      character(len=*), intent(in) :: state_type
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      type(ESMF_FieldBundle) :: out_bundle
      character(len=ESMF_MAXSTR) :: file_name
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      if (item_count > 0) then
         file_name = trim(this%gc_name) // "_" // trim(state_type) // "_checkpoint.nc4"
         print *, "Writing restart: ", trim(file_name)
         out_bundle = get_bundle_from_state_(state, _RC)
         call this%write_bundle_(out_bundle, file_name, rc)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine write

   subroutine read(this, state_type, state, rc)

      ! Arguments
      class(Restart), intent(inout) :: this
      character(len=*), intent(in) :: state_type
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      character(len=ESMF_MAXSTR) :: file_name
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      if (item_count > 0) then
         file_name = trim(this%gc_name) // "_" // trim(state_type) // "_rst.nc4"
         print *, "Reading restart: ", trim(file_name)
         call read_fields_(file_name, state, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine read

   type(ESMF_FieldBundle) function get_bundle_from_state_(state, rc) result(bundle)
      ! Arguments
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      character(len=ESMF_MAXSTR), allocatable :: item_name(:)
      type (ESMF_StateItem_Flag), allocatable  :: item_type(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: item_count, idx, status

      bundle = ESMF_FieldBundleCreate(_RC) ! bundle to pack fields in
      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(item_name(item_count), stat=status); _VERIFY(status)
      allocate(item_type(item_count), stat=status); _VERIFY(status)
      call ESMF_StateGet(state, itemNameList=item_name, itemTypeList=item_type, _RC)
      do idx = 1, item_count
         if (item_type(idx) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, item_name(idx), field, _RC)
            call ESMF_FieldGet(field, status=field_status, _RC)
            if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
               call ESMF_FieldBundleAdd(bundle, [field], _RC)
            end if
         else if (item_type(idx) == ESMF_STATEITEM_FIELDBUNDLE) then
            print *, "FieldBundle: ", trim(item_name(idx))
            error stop "Not implemented yet"
         end if
      end do
      deallocate(item_name, item_type, stat=status); _VERIFY(status)

      _RETURN(ESMF_SUCCESS)
   end function get_bundle_from_state_

   subroutine write_bundle_(this, bundle, file_name, rc)
      ! Arguments
      class(Restart), intent(in) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: file_name
      integer, optional, intent(out) :: rc

      ! Locals
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: writer
      type(MaplGeom), pointer :: mapl_geom
      integer :: status

      metadata = bundle_to_metadata(bundle, this%gc_geom, _RC)
      allocate(writer, source=make_geom_pfio(metadata, rc=status)); _VERIFY(status)
      mapl_geom => get_mapl_geom(this%gc_geom, _RC)
      call writer%initialize(metadata, mapl_geom, _RC)
      call writer%update_time_on_server(this%current_time, _RC)
      ! TODO: no-op if bundle is empty, or should we skip empty bundles?
      call writer%stage_data_to_file(bundle, file_name, 1, _RC)
      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()
      deallocate(writer)

      _RETURN(ESMF_SUCCESS)
   end subroutine write_bundle_

   subroutine read_fields_(file_name, state, rc)
      ! Arguments
      character(len=*), intent(in) :: file_name
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      logical :: file_exists
      integer :: status

      inquire(file=trim(file_name), exist=file_exists)
      _ASSERT(file_exists, "restart file " // trim(file_name) // " does not exist")

      call request_data_from_file(state, file_name, _RC)
      call i_Clients%done_collective_prefetch()
      call i_Clients%wait()

      _RETURN(ESMF_SUCCESS)
   end subroutine read_fields_

   ! pchakrab: TODO - this should probably go to Grid_PFIO.F90
   subroutine request_data_from_file(state, file_name, rc)
      ! Arguments
      type(ESMF_State), intent(in) :: state
      character(len=*), intent(in) :: file_name
      integer, intent(out), optional :: rc

      ! Locals
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetaData) :: metadata
      character(len=ESMF_MAXSTR), allocatable :: item_name(:)
      type (ESMF_StateItem_Flag), allocatable  :: item_type(:)
      type(ESMF_Grid) :: grid
      type(ESMF_Field) :: field
      type(ESMF_TypeKind_Flag) :: esmf_typekind
      integer :: pfio_typekind
      integer, allocatable :: element_count(:), new_element_count(:)
      integer, allocatable :: local_start(:), global_start(:), global_count(:)
      type(c_ptr) :: address
      type(pFIOServerBounds) :: server_bounds
      type(ArrayReference) :: ref
      integer :: collection_id, num_fields, idx, status

      call file_formatter%open(file_name, PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)

      call ESMF_StateGet(state, itemCount=num_fields, _RC)
      allocate(item_name(num_fields), stat=status); _VERIFY(status)
      allocate(item_type(num_fields), stat=status); _VERIFY(status)
      call ESMF_StateGet(state, itemNameList=item_name, itemTypeList=item_type, _RC)
      collection_id = i_Clients%add_ext_collection(file_name, _RC)
      do idx = 1, num_fields
         if (item_type(idx) /= ESMF_STATEITEM_FIELD) then
            error stop "cannot read non-ESMF_STATEITEM_FIELD type"
         end if
         associate (var_name => item_name(idx))
           _ASSERT(metadata%has_variable(var_name), "var not in file metadata")
           call ESMF_StateGet(state, var_name, field, _RC)
           call ESMF_FieldGet(field, grid=grid, typekind=esmf_typekind, _RC)
           element_count = FieldGetLocalElementCount(field, _RC)
           call server_bounds%initialize(grid, element_count, _RC)
           global_start = server_bounds%get_global_start()
           global_count = server_bounds%get_global_count()
           local_start = server_bounds%get_local_start()
           call FieldGetCptr(field, address, _RC)
           pfio_typekind = esmf_to_pfio_type(esmf_typekind, _RC)
           new_element_count = server_bounds%get_file_shape()
           ref = ArrayReference(address, pfio_typekind, new_element_count)
           call i_Clients%collective_prefetch_data( &
                collection_id, &
                file_name, &
                var_name, &
                ref, &
                start=local_start, &
                global_start=global_start, &
                global_count=global_count)
           call server_bounds%finalize()
         end associate
      end do

      _RETURN(ESMF_SUCCESS)
   end subroutine request_data_from_file

end module mapl3g_Restart

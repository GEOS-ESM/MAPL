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
      procedure, private :: read_fields_
   end type Restart

   interface Restart
      procedure new_Restart
   end interface Restart

contains

   function new_Restart(gc_name, gc_geom, gc_clock, rc) result(new_rstrt)
      character(len=*), intent(in) :: gc_name
      type(ESMF_Geom), intent(in) :: gc_geom
      type(ESMF_Clock), intent(in) :: gc_clock
      integer, optional, intent(out) :: rc
      type(Restart) :: new_rstrt ! result

      integer :: status

      new_rstrt%gc_name = ESMF_UtilStringLowerCase(trim(gc_name), _RC)
      call ESMF_Clockget(gc_clock, currTime = new_rstrt%current_time, _RC)
      new_rstrt%gc_geom = gc_geom

      _RETURN(ESMF_SUCCESS)
   end function new_Restart

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
         print *, "Writing checkpoint: ", trim(file_name)
         out_bundle = get_bundle_from_state_(state, _RC)
         call this%write_bundle_(out_bundle, file_name, rc)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine write

   subroutine read(this, state_type, state, rc)

      ! Arguments
      class(Restart), intent(inout) :: this
      character(len=*), intent(in) :: state_type
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      character(len=ESMF_MAXSTR) :: file_name
      logical :: file_exists
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      if (item_count > 0) then
         file_name = trim(this%gc_name) // "_" // trim(state_type) // "_rst.nc4"
         inquire(file=trim(file_name), exist=file_exists)
         if (file_exists) then
            print *, "Reading restart: ", trim(file_name)
            call this%read_fields_(file_name, state, _RC)
         else
            print *, "Restart file <" // trim(file_name) // "> does not exist. Skip reading!"
         end if
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

   subroutine read_fields_(this, file_name, state, rc)
      ! Arguments
      class(Restart), intent(in) :: this
      character(len=*), intent(in) :: file_name
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: reader
      type(MaplGeom), pointer :: mapl_geom
      integer :: status

      call file_formatter%open(file_name, PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)
      allocate(reader, source=make_geom_pfio(metadata, rc=status)); _VERIFY(status)
      mapl_geom => get_mapl_geom(this%gc_geom, _RC)
      call reader%initialize(file_name, mapl_geom, _RC)
      call reader%request_data_from_file(file_name, state, _RC)
      call i_Clients%done_collective_prefetch()
      call i_Clients%wait()

      _RETURN(ESMF_SUCCESS)
   end subroutine read_fields_

end module mapl3g_Restart

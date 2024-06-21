#include "MAPL_Generic.h"

module mapl3g_Restart

   use esmf
   use pFIO, only: FileMetaData, o_Clients
   use mapl3g_geom_mgr, only: MaplGeom, get_geom_manager
   use mapl3g_MultiState, only: MultiState
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return
   use mapl3g_geomio, only: bundle_to_metadata, GeomPFIO, make_geom_pfio, get_mapl_geom

   implicit none
   private

   public :: Restart

   type :: Restart
      private
   contains
      procedure, public :: write
      procedure, public :: read
   end type Restart

contains

   subroutine write(this, gc_name, gc_states, gc_geom, clock, rc)
      ! Arguments
      class(Restart), intent(inout) :: this
      character(len=*), intent(in) :: gc_name
      type(MultiState), intent(in) :: gc_states
      type(ESMF_Geom), intent(in) :: gc_geom
      type(ESMF_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      ! Locals
      type(ESMF_State) :: export_state
      type(ESMF_FieldBundle) :: out_bundle
      type(ESMF_Time) :: current_time
      character(len=ESMF_MAXSTR) :: gc_name_lowercase
      character(len=ESMF_MAXSTR) :: file_name
      integer :: status

      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call gc_states%get_state(export_state, "export", _RC)
      out_bundle = get_bundle_from_state_(export_state, _RC)
      gc_name_lowercase = ESMF_UtilStringLowerCase(trim(gc_name), _RC)
      file_name = trim(gc_name_lowercase) // "_export_rst.nc4"
      call write_bundle_(out_bundle, file_name, gc_geom, current_time, rc)

      _RETURN(ESMF_SUCCESS)
   end subroutine write

   subroutine read(this, rc)

      ! Arguments
      class(Restart), intent(inout) :: this
      integer, optional, intent(out) :: rc

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

   subroutine write_bundle_(bundle, file_name, geom, current_time, rc)
      ! Arguments
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: file_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      ! Locals
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: writer
      type(MaplGeom), pointer :: mapl_geom
      character(len=ESMF_MAXSTR) :: filename
      integer :: status

      metadata = bundle_to_metadata(bundle, geom, _RC)
      allocate(writer, source=make_geom_pfio(metadata, rc=status)); _VERIFY(status)
      mapl_geom => get_mapl_geom(geom, _RC)
      call writer%initialize(metadata, mapl_geom, _RC)
      call writer%update_time_on_server(current_time, _RC)
      ! TODO: no-op if bundle is empty, or should we skip empty bundles?
      call writer%stage_data_to_file(bundle, file_name, 1, _RC)
      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()
      deallocate(writer)

      _RETURN(ESMF_SUCCESS)
   end subroutine write_bundle_

end module mapl3g_Restart

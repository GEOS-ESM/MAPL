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
      procedure :: wr1te
      procedure :: r3ad
   end type Restart

contains

   type(ESMF_FieldBundle) function bundle_from_state_(state, rc) result(bundle)
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
   end function bundle_from_state_

   subroutine wr1te(this, name, states, geom, clock, rc)
      ! Arguments
      class(Restart), intent(inout) :: this
      character(len=*), intent(in) :: name
      type(MultiState), intent(in) :: states
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      ! Locals
      type(ESMF_State) :: export_state
      type(ESMF_FieldBundle) :: out_bundle
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: writer
      type(MaplGeom), pointer :: mapl_geom
      type(ESMF_Time) :: current_time
      character(len=ESMF_MAXSTR) :: filename
      integer :: status

      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      ! call ESMF_TimePrint(current_time)
      call states%get_state(export_state, "export", _RC)
      out_bundle = bundle_from_state_(export_state, _RC)
      metadata = bundle_to_metadata(out_bundle, geom, _RC)
      allocate(writer, source=make_geom_pfio(metadata, rc=status)); _VERIFY(status)
      mapl_geom => get_mapl_geom(geom, _RC)
      call writer%initialize(metadata, mapl_geom, _RC)
      call writer%update_time_on_server(current_time, _RC)
      filename = ESMF_UtilStringLowerCase(trim(name), rc=status) // "_export_rst.nc4"
      _VERIFY(status)
      ! no-op if bundle is empty
      call writer%stage_data_to_file(out_bundle, filename, 1, _RC)
      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()
      deallocate(writer)

      _RETURN(ESMF_SUCCESS)
   end subroutine wr1te

   subroutine r3ad(this, rc)

      ! Arguments
      class(Restart), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine r3ad
      
end module mapl3g_Restart

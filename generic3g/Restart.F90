#include "MAPL_Generic.h"

module mapl3g_Restart

   ! use mapl3g_geom_mgr, only: GeomManager, MaplGeom, get_geom_manager
   ! use mapl3g_UserSetServices,   only: AbstractUserSetServices
   ! use mapl3g_VariableSpec
   ! use mapl3g_StateItem
   use mapl3g_MultiState, only: MultiState
   ! use mapl3g_VariableSpecVector
   ! use mapl3g_ComponentSpec
   ! use mapl3g_GenericPhases
   ! use mapl3g_Validation, only: is_valid_name
   ! use mapl3g_InnerMetaComponent
   ! use mapl3g_MethodPhasesMap
   ! use mapl3g_StateItemSpec
   ! use mapl3g_ConnectionPt
   ! use mapl3g_MatchConnection
   ! use mapl3g_VirtualConnectionPt
   ! use mapl3g_ActualPtVector
   ! use mapl3g_ConnectionVector
   ! use mapl3g_HierarchicalRegistry
   ! use mapl3g_StateExtension
   ! use mapl3g_ExtensionVector
   ! use mapl3g_ESMF_Interfaces, only: I_Run, MAPL_UserCompGetInternalState, MAPL_UserCompSetInternalState
   ! use mapl3g_ComponentDriver
   ! use mapl3g_GriddedComponentDriver
   ! use mapl3g_GriddedComponentDriverMap, only: GriddedComponentDriverMap
   ! use mapl3g_GriddedComponentDriverMap, only: GriddedComponentDriverMapIterator
   ! use mapl3g_GriddedComponentDriverMap, only: operator(/=)
   ! use mapl3g_ActualPtComponentDriverMap
   ! use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_INVALIDATE
   ! use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_UPDATE
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return
   ! use mapl3g_VerticalGeom
   ! use mapl3g_GeometrySpec
   ! use gFTL2_StringVector
   ! use mapl_keywordEnforcer, only: KE => KeywordEnforcer

   use esmf
   ! use pflogger, only: logging, Logger
   ! use pFIO, only: FileMetaData, o_Clients
   ! use mapl3g_geomio, only: GeomPFIO, bundle_to_metadata, make_geom_pfio, get_mapl_geom

   implicit none
   private

   public :: Restart
   public :: bundle_from_state_

   type :: Restart
      private
   contains
      procedure :: write
      procedure :: read
   end type Restart

   ! interface Restart
   !    module procedure new_Restart
   ! end interface Restart

contains


   ! ! Constructor
   ! type(Restart) function new_Restart() result(restart)
   ! end function new_Restart

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
            ! print *, "Field name: ", trim(item_name(idx))
            if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
               ! call ESMF_FieldGet(field, typekind=field_type, _RC)
               ! print *, "Field type: ", field_type
               ! call ESMF_FieldPrint(field, _RC)
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

   subroutine write(this, states, rc)

      ! Arguments
      class(Restart), intent(inout) :: this
      type(MultiState), intent(in) :: states
      integer, optional, intent(out) :: rc

      ! Locals
      type(ESMF_FieldBundle) :: o_bundle
      type(ESMF_State) :: export_state
      integer :: status
      ! type(FileMetaData) :: metadata
      ! class(GeomPFIO), allocatable :: writer
      ! type(GeomManager), pointer :: geom_mgr
      ! type(MaplGeom), pointer :: mapl_geom
      ! type(ESMF_Time) :: current_time
      ! character(len=ESMF_MAXSTR) :: current_file

      ! integer :: status, item_count, idx

      call states%get_state(export_state, "export", _RC)
      o_bundle = bundle_from_state_(export_state, _RC)

      ! child_outer_gc = child%get_gridcomp()
      ! child_meta => get_outer_meta(child_outer_gc, _RC)
      ! child_geom = child_meta%get_geom()
      ! metadata = bundle_to_metadata(o_bundle, child_geom, _RC)
      ! allocate(writer, source=make_geom_pfio(metadata, rc=status)); _VERIFY(status)
      ! mapl_geom => get_mapl_geom(child_geom, _RC)
      ! call writer%initialize(metadata, mapl_geom, _RC)
      ! call ESMF_ClockGet(clock, currTime=current_time, _RC)
      ! call ESMF_TimePrint(current_time)
      ! call writer%update_time_on_server(current_time, _RC)
      ! current_file = ESMF_UtilStringLowerCase(trim(child_name), rc=status) // "_export_rst.nc4"
      ! _VERIFY(status)
      ! print *, "Current file: ", trim(current_file)
      ! ! no-op if bundle is empty
      ! call writer%stage_data_to_file(o_bundle, current_file, 1, _RC)
      ! call o_Clients%done_collective_stage()
      ! call o_Clients%post_wait()
      ! deallocate(writer)

      _RETURN(ESMF_SUCCESS)
   end subroutine write

   subroutine read(this, rc)

      ! Arguments
      class(Restart), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine read
      
end module mapl3g_Restart

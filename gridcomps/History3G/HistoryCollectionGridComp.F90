#include "MAPL_Generic.h"

module mapl3g_HistoryCollectionGridComp
   use mapl_ErrorHandlingMod
   use generic3g
   use mapl3g_esmf_utilities
   use mapl3g_HistoryCollectionGridComp_private
   use esmf
   use mapl3g_BundleWriter
   implicit none
   private

   public :: setServices

   ! Private state
   type :: HistoryCollectionGridComp
!#      class(Client), pointer :: client
      type(ESMF_FieldBundle) :: output_bundle
      type(ESMF_Alarm) :: write_alarm
      type(ESMF_Time) :: start_stop_times(2)
   end type HistoryCollectionGridComp


contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(HistoryCollectionGridComp), pointer :: collection_gridcomp
      type(ESMF_HConfig) :: hconfig
      character(*), parameter :: PRIVATE_STATE = "HistoryCollectionGridComp"
      integer :: status

      type(VerticalGeom) :: vertical_geom
      type(OuterMetaComponent), pointer :: outer_meta

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init_geom, phase_name='GENERIC::INIT_ADVERTISE_GEOM', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)

      outer_meta => get_outer_meta_from_inner_gc(gridcomp,_RC)
      vertical_geom = VerticalGeom(4)
      call outer_meta%set_vertical_geom(vertical_geom)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      call register_imports(gridcomp,hconfig,_RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status
      character(*), parameter :: PRIVATE_STATE = "HistoryCollectionGridComp"
      type(HistoryCollectionGridComp), pointer :: collection_gridcomp
      type(ESMF_HConfig) :: hconfig
      type(ESMF_Geom) :: geom
      type(ESMF_Alarm) :: alarm

      ! To Do:
      ! - determine run frequencey and offset (save as alarm)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)
      collection_gridcomp%output_bundle = create_output_bundle(hconfig, importState, _RC)

      call MAPL_GridCompGet(gridcomp, geom=geom, _RC)

      collection_gridcomp%write_alarm = create_output_alarm(clock, hconfig, _RC)
      collection_gridcomp%start_stop_times = set_start_stop_time(clock, hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine init


   subroutine init_geom(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status
      type(ESMF_HConfig) :: hconfig
      type(ESMF_Geom) :: geom

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      geom = make_geom(hconfig)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine init_geom

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status
      type(HistoryCollectionGridComp), pointer :: collection_gridcomp
      character(*), parameter :: PRIVATE_STATE = "HistoryCollectionGridComp"
      logical :: time_to_write, run_collection
      type(ESMF_Time) :: current_time
    
      call ESMF_ClockGet(clock, currTime=current_time, _RC) 
      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)
      time_to_write = ESMF_AlarmIsRinging(collection_gridcomp%write_alarm, _RC)
      run_collection = (current_time >= collection_gridcomp%start_stop_times(1)) .and. &
                           (current_time <= collection_gridcomp%start_stop_times(2))

      _RETURN_UNLESS(run_collection .and. time_to_write)

      _RETURN(_SUCCESS)

   end subroutine run

end module mapl3g_HistoryCollectionGridComp

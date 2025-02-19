#include "MAPL_Generic.h"

module mapl3g_HistoryCollectionGridComp
   use mapl_ErrorHandlingMod
   use generic3g
   use mapl3g_esmf_utilities
   use mapl3g_HistoryCollectionGridComp_private
   use mapl3g_BasicVerticalGrid
   use mapl3g_geomio
   use mapl3g_geom_mgr
   use mapl_StringTemplate
   use pfio
   use esmf
   
   implicit none
   private

   public :: setServices

   ! Private state
   type :: HistoryCollectionGridComp
      type(ESMF_FieldBundle) :: output_bundle
      class(GeomPFIO), allocatable :: writer
      type(ESMF_Time) :: start_stop_times(2)
      type(ESMF_Time) :: initial_file_time
      character(len=:), allocatable :: template
      character(len=:), allocatable :: current_file
   end type HistoryCollectionGridComp

   character(len=*), parameter :: null_file = 'null_file'

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig
      character(*), parameter :: PRIVATE_STATE = "HistoryCollectionGridComp"
      integer :: status

      type(BasicVerticalGrid) :: vertical_grid

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init_geom, phase_name='GENERIC::INIT_ADVERTISE', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE)

      vertical_grid = BasicVerticalGrid(4)
      call MAPL_GRidCompSetVerticalGrid(gridcomp, vertical_grid, _RC)
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
      character(len=ESMF_MAXSTR) :: name
      type(FileMetadata) :: metadata
      type(MaplGeom), pointer :: mapl_geom

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      call ESMF_GridCompGet(gridcomp, name=name, _RC)

      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)
      collection_gridcomp%output_bundle = create_output_bundle(hconfig, importState, _RC)

      call MAPL_GridCompGet(gridcomp, geom=geom, _RC)
      metadata = bundle_to_metadata(collection_gridcomp%output_bundle, geom, _RC)
      mapl_geom => get_mapl_geom(geom, _RC)
      allocate(collection_gridcomp%writer, source=make_geom_pfio(metadata, rc=status))
      _VERIFY(STATUS)
      call collection_gridcomp%writer%initialize(metadata, mapl_geom, _RC)

      call create_output_alarm(clock, hconfig, trim(name), _RC)
      collection_gridcomp%start_stop_times = set_start_stop_time(clock, hconfig, _RC)

      collection_gridcomp%current_file = null_file
      collection_gridcomp%template = ESMF_HConfigAsString(hconfig, keyString='template', _RC)
      

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

      integer :: status, time_index
      type(HistoryCollectionGridComp), pointer :: collection_gridcomp
      character(*), parameter :: PRIVATE_STATE = "HistoryCollectionGridComp"
      logical :: time_to_write, run_collection
      type(ESMF_Time) :: current_time
      type(ESMF_TimeInterval) :: write_frequency
      type(ESMF_Alarm) :: write_alarm
      character(len=ESMF_MAXSTR) :: name
      character(len=128) :: current_file

      call ESMF_GridCompGet(gridcomp, name=name, _RC)
      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call ESMF_ClockGetAlarm(clock, trim(name)//"_write_alarm", write_alarm, _RC)
      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)
      time_to_write = ESMF_AlarmIsRinging(write_alarm, _RC)
      run_collection = (current_time >= collection_gridcomp%start_stop_times(1)) .and. &
                           (current_time <= collection_gridcomp%start_stop_times(2))

      _RETURN_UNLESS(run_collection .and. time_to_write)

      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)

      call ESMF_AlarmGet(write_alarm, ringInterval=write_frequency, _RC)
      call fill_grads_template_esmf(current_file, collection_gridcomp%template, collection_id=name, time=current_time, _RC)
      if (trim(current_file) /= collection_gridcomp%current_file) then
         collection_gridcomp%current_file = current_file
         call collection_gridcomp%writer%update_time_on_server(current_time, _RC)
         collection_gridcomp%initial_file_time = current_time
      end if

      time_index = get_current_time_index(collection_gridcomp%initial_file_time, current_time, write_frequency)
      call collection_gridcomp%writer%stage_data_to_file(collection_gridcomp%output_bundle, collection_gridcomp%current_file, time_index, _RC)
      _RETURN(_SUCCESS)

   end subroutine run

end module mapl3g_HistoryCollectionGridComp

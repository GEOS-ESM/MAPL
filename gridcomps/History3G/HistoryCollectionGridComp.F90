#include "MAPL.h"

module mapl3g_HistoryCollectionGridComp
   use mapl3
   use mapl3g_HistoryCollectionGridComp_private
   use esmf
   use MAPL_StringTemplate, only: fill_grads_template_esmf
   implicit none
   private

   public :: setServices

   ! Private state
   type :: HistoryCollectionGridComp
      type(ESMF_FieldBundle) :: output_bundle
      class(GeomPFIO), allocatable :: writer
      type(ESMF_Time) :: start_stop_times(2)
      type(ESMF_Time) :: initial_file_time
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_TimeInterval) :: time_offstep
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

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init_geom, phase_name='GENERIC::INIT_ADVERTISE', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE)

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
      character(len=ESMF_MAXSTR) :: name
      type(FileMetadata) :: metadata
      type(MaplGeom), pointer :: mapl_geom

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      call ESMF_GridCompGet(gridcomp, name=name, _RC)

      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)
      collection_gridcomp%output_bundle = create_output_bundle(hconfig, importState, _RC)

      geom = detect_geom(collection_gridcomp%output_bundle, name, _RC)
      metadata = bundle_to_metadata(collection_gridcomp%output_bundle, geom, _RC)
      mapl_geom => get_mapl_geom(geom, _RC)
      allocate(collection_gridcomp%writer, source=make_geom_pfio(metadata, rc=status))
      _VERIFY(STATUS)
      call collection_gridcomp%writer%initialize(metadata, mapl_geom, _RC)

      collection_gridcomp%start_stop_times = set_start_stop_time(clock, hconfig, _RC)
      collection_gridcomp%timeStep = get_frequency(hconfig, _RC)
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
      logical :: has_geom

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      has_geom = ESMF_HConfigIsDefined(hconfig, keystring='geom', _RC)
      if (has_geom) then
         geom = make_geom(hconfig)
         call MAPL_GridCompSetGeom(gridcomp, geom, _RC)
      end if

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
      logical :: run_collection
      type(ESMF_Time) :: current_time
      character(len=ESMF_MAXSTR) :: name
      character(len=128) :: current_file
      real, allocatable :: current_time_vector(:)

      call ESMF_GridCompGet(gridcomp, name=name, _RC)
      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)

      run_collection = (current_time >= collection_gridcomp%start_stop_times(1)) .and. &
                           (current_time <= collection_gridcomp%start_stop_times(2))

      _RETURN_UNLESS(run_collection)

      _GET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)

      call fill_grads_template_esmf(current_file, collection_gridcomp%template, collection_id=name, time=current_time, _RC)
      if (trim(current_file) /= collection_gridcomp%current_file) then
         collection_gridcomp%current_file = current_file
         call collection_gridcomp%writer%update_time_on_server(current_time, _RC)
         collection_gridcomp%initial_file_time = current_time
      end if

      call get_current_time_info(collection_gridcomp%initial_file_time, current_time, collection_gridcomp%timeStep, time_index, current_time_vector, _RC)
      call collection_gridcomp%writer%stage_time_to_file(collection_gridcomp%current_file, current_time_vector,  _RC)
      call collection_gridcomp%writer%stage_data_to_file(collection_gridcomp%output_bundle, collection_gridcomp%current_file, time_index, _RC)
      _RETURN(_SUCCESS)

   end subroutine run

end module mapl3g_HistoryCollectionGridComp

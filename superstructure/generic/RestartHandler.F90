#include "MAPL.h"

module mapl_RestartHandler_mod

   use esmf
   use mapl_ErrorHandling_mod, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use mapl_SharedIO_mod, only: bundle_to_metadata
   use mapl_GeomPFIO_mod, only: GeomPFIO
   use mapl_GeomCategorizer_mod, only: make_geom_pfio
   use mapl_FieldInfo_mod, only: FieldInfoGetInternal
    use mapl_RestartModes_mod, only: RestartMode, operator(==), RESTART_SKIP
    use mapl_state_api, only: MAPL_StateGet
    use mapl_field_bundle_api, only: MAPL_FieldBundleFilter
    use mapl_DefaultServerNames_mod, only: MAPL_DEFAULT_INPUT_SERVER, MAPL_DEFAULT_OUTPUT_SERVER
    use pFIO, only: PFIO_READ, FileMetaData, NetCDF4_FileFormatter
    use pFIO, only: get_client, ClientThread
   use pFlogger, only: logging, logger

   implicit none(type,external)
   private

   public :: RestartHandler

   type :: RestartHandler
      private
      type(ESMF_Geom) :: gridcomp_geom
      type(ESMF_Time) :: current_time
      class(logger), pointer :: lgr => null()
   contains
      procedure, public :: write
      procedure, public :: read
      procedure, private :: write_bundle_
      procedure, private :: read_bundle_
   end type RestartHandler

   ! Selects which fields of state are eligible for a given restart
   ! operation ('write' or 'read'), and returns them flattened into a
   ! single bundle. Shared by RestartHandler%write/%read (netCDF path)
   ! and by the in-memory checkpoint write/read (OuterMetaComponent).
   public :: get_restart_bundle

   interface RestartHandler
      procedure new_RestartHandler
   end interface RestartHandler

contains

   function new_RestartHandler(gridcomp_geom, current_time, gridcomp_logger) result(restart_handler)
      type(ESMF_Geom), intent(in) :: gridcomp_geom
      type(ESMF_Time), intent(in) :: current_time
      class(logger), pointer, optional, intent(in) :: gridcomp_logger
      type(RestartHandler) :: restart_handler ! result

      restart_handler%gridcomp_geom = gridcomp_geom
      restart_handler%current_time = current_time
      restart_handler%lgr => logging%get_logger('mapl.restart')
      if (present(gridcomp_logger)) restart_handler%lgr => gridcomp_logger
   end function new_RestartHandler

   subroutine write(this, state, filename, rc)
      class(RestartHandler), intent(inout) :: this
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: bundle
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      _RETURN_UNLESS(item_count>0)

      call this%lgr%info("Writing checkpoint: %a", filename)
      call get_restart_bundle(state, is_write=.true., bundle=bundle, _RC)
      call this%write_bundle_(bundle, filename, _RC)
      call ESMF_FieldBundleDestroy(bundle, _RC)

      _RETURN(_SUCCESS)
   end subroutine write

   subroutine read(this, state, filename, bootstrap, rc)
      class(RestartHandler), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: filename
      logical, intent(in) :: bootstrap
      integer, optional, intent(out) :: rc

      logical :: file_exists
      type(ESMF_FieldBundle) :: bundle
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      _RETURN_UNLESS(item_count>0)

      inquire(file=filename, exist=file_exists)
      _RETURN_IF(bootstrap .and. (.not. file_exists))
      _ASSERT(file_exists, "Restart file " // trim(filename) // " does not exist")
      call this%lgr%info("Reading restart: %a", trim(filename))
      call get_restart_bundle(state, is_write=.false., bundle=bundle, _RC)
      call this%read_bundle_(filename, bundle, _RC)
      call ESMF_FieldBundleDestroy(bundle, _RC)

      _RETURN(_SUCCESS)
   end subroutine read

   subroutine write_bundle_(this, bundle, filename, rc)
      class(RestartHandler), intent(in) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: writer
      integer :: status
      class(ClientThread), pointer :: o_client

      metadata = bundle_to_metadata(bundle, this%gridcomp_geom, _RC)
      allocate(writer, source=make_geom_pfio(metadata), _STAT)
      call writer%initialize(metadata, this%gridcomp_geom, _RC)
      call writer%update_time_on_server(this%current_time, _RC)
      ! TODO: no-op if bundle is empty, or should we skip empty bundles?
      call writer%stage_coordinates_to_file(filename, _RC)
      call writer%stage_data_to_file(bundle, filename, 1, _RC)
       o_client => get_client(MAPL_DEFAULT_OUTPUT_SERVER, _RC)
      call o_client%done_collective_stage()
      call o_client%post_wait_all()

      _RETURN(_SUCCESS)
   end subroutine write_bundle_

   subroutine read_bundle_(this, filename, bundle, rc)
      class(RestartHandler), intent(in) :: this
      character(len=*), intent(in) :: filename
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: reader
      integer :: status
      class(ClientThread), pointer :: i_client

      call file_formatter%open(filename, PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)
      allocate(reader, source=make_geom_pfio(metadata), _STAT)
      call reader%initialize(filename, this%gridcomp_geom, _RC)
      call reader%request_data_from_file(filename, bundle, _RC)
       i_client => get_client(MAPL_DEFAULT_INPUT_SERVER, _RC)
      call i_client%done_collective_prefetch()
      call i_client%wait_all()

      _RETURN(_SUCCESS)
   end subroutine read_bundle_

   ! Build the restart-eligible flattened bundle for a given state.
   ! Shared by RestartHandler%write/%read (netCDF path) and by the
   ! in-memory checkpoint write/read (OuterMetaComponent), so both
   ! paths use identical field-selection semantics:
   !  - write (is_write=.true.): drop fields that are not
   !    ESMF_FIELDSTATUS_COMPLETE.
   !  - read (is_write=.false.): drop fields marked RESTART_SKIP.
   subroutine get_restart_bundle(state, is_write, bundle, rc)
      type(ESMF_State), intent(in) :: state
      logical, intent(in) :: is_write
      type(ESMF_FieldBundle), intent(out) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_StateGet(state, bundle, _RC)
      if (is_write) then
         call MAPL_FieldBundleFilter(bundle, predicate_incomplete_, _RC)
      else
         call MAPL_FieldBundleFilter(bundle, predicate_skip_restart_, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine get_restart_bundle

   function predicate_skip_restart_(field, rc) result(remove)
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      logical :: remove

      type(ESMF_Info) :: info
      type(RestartMode) :: restart_mode
      integer :: alias_id, status

      call ESMF_InfoGetFromHost(field, info, _RC)
      call ESMF_NamedAliasGet(field, id=alias_id, _RC)
      call FieldInfoGetInternal(info, alias_id, restart_mode, _RC)
      remove = (restart_mode == RESTART_SKIP)

      _RETURN(_SUCCESS)
   end function predicate_skip_restart_

   function predicate_incomplete_(field, rc) result(remove)
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      logical :: remove

      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: status

      call ESMF_FieldGet(field, status=field_status, _RC)
      remove = (field_status /= ESMF_FIELDSTATUS_COMPLETE)

      _RETURN(_SUCCESS)
   end function predicate_incomplete_

end module mapl_RestartHandler_mod

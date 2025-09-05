#include "MAPL.h"

module mapl3g_RestartHandler

   use, intrinsic :: iso_c_binding, only: c_ptr
   use esmf
   use mapl3g_Geom_API, only: MaplGeom
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use mapl3g_geomio, only: bundle_to_metadata, GeomPFIO, make_geom_pfio, get_mapl_geom
   use mapl3g_FieldInfo, only: FieldInfoGetInternal
   use mapl3g_RestartModes, only: RestartMode, operator(==), MAPL_RESTART_SKIP
   use pFIO, only: PFIO_READ, FileMetaData, NetCDF4_FileFormatter
   use pFIO, only: i_Clients, o_Clients
   use pFlogger, only: logging, logger

   implicit none(type, external)
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
      procedure, private :: filter_fields_create_bundle_
   end type RestartHandler

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
      bundle = this%filter_fields_create_bundle_(state, _RC)
      call this%write_bundle_(bundle, filename, rc)
      call ESMF_FieldBundleDestroy(bundle, _RC)

      _RETURN(_SUCCESS)
   end subroutine write

   subroutine read(this, state, filename, rc)
      class(RestartHandler), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      logical :: file_exists
      type(ESMF_FieldBundle) :: bundle
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      _RETURN_UNLESS(item_count>0)

      inquire(file=filename, exist=file_exists)
      if (.not. file_exists) then
         ! TODO: Need to decide what happens in that case. Bootstrapping variables?
         call this%lgr%warning("Restart file << %a >> does not exist. Skip reading!", filename)
         _RETURN(_SUCCESS)
      end if
      call this%lgr%info("Reading restart: %a", trim(filename))
      bundle = this%filter_fields_create_bundle_(state, _RC)
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
      type(MaplGeom), pointer :: mapl_geom
      integer :: status

      metadata = bundle_to_metadata(bundle, this%gridcomp_geom, _RC)
      allocate(writer, source=make_geom_pfio(metadata), _STAT)
      mapl_geom => get_mapl_geom(this%gridcomp_geom, _RC)
      call writer%initialize(metadata, mapl_geom, _RC)
      call writer%update_time_on_server(this%current_time, _RC)
      ! TODO: no-op if bundle is empty, or should we skip empty bundles?
      call writer%stage_data_to_file(bundle, filename, 1, _RC)
      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()

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
      type(MaplGeom), pointer :: mapl_geom
      integer :: status

      call file_formatter%open(filename, PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)
      allocate(reader, source=make_geom_pfio(metadata), _STAT)
      mapl_geom => get_mapl_geom(this%gridcomp_geom, _RC)
      call reader%initialize(filename, mapl_geom, _RC)
      call reader%request_data_from_file(filename, bundle, _RC)
      call i_Clients%done_collective_prefetch()
      call i_Clients%wait()

      _RETURN(_SUCCESS)
   end subroutine read_bundle_

   function filter_fields_create_bundle_(this, state, rc) result(bundle)
      class(RestartHandler), intent(in) :: this
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc
      type(ESMF_FieldBundle) :: bundle ! result

      type(ESMF_Field) :: field
      character(len=ESMF_MAXSTR), allocatable :: names(:)
      type (ESMF_StateItem_Flag), allocatable  :: types(:)
      type(ESMF_Info) :: info
      integer :: alias_id
      type(RestartMode) :: restart_mode
      integer :: idx, num_fields, status

      call ESMF_StateGet(state, itemCount=num_fields, _RC)
      allocate(names(num_fields), _STAT)
      allocate(types(num_fields), _STAT)
      call ESMF_StateGet(state, itemNameList=names, itemTypeList=types, _RC)
      bundle = ESMF_FieldBundleCreate(_RC)
      do idx = 1, num_fields
         if (types(idx) /= ESMF_STATEITEM_FIELD) then
            call this%lgr%warning("Item [ %a ] is not a field! Not handled at the moment", trim(names(idx)))
            cycle
         end if
         call ESMF_StateGet(state, names(idx), field, _RC)
         call ESMF_InfoGetFromHost(field, info, _RC)
         call ESMF_NamedAliasGet(field, id=alias_id, _RC)
         call FieldInfoGetInternal(info, alias_id, restart_mode, _RC)
         if (restart_mode==MAPL_RESTART_SKIP) cycle
         call ESMF_FieldBundleAdd(bundle, [field], _RC)
      end do

      _RETURN(_SUCCESS)
   end function filter_fields_create_bundle_

end module mapl3g_RestartHandler

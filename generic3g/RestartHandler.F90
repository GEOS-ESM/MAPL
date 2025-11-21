#include "MAPL.h"

module mapl3g_RestartHandler

   use, intrinsic :: iso_c_binding, only: c_ptr
   use esmf
   use mapl3g_Geom_API, only: MaplGeom
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use mapl3g_geomio, only: bundle_to_metadata, GeomPFIO, make_geom_pfio, get_mapl_geom
   use mapl3g_FieldInfo, only: FieldInfoGetInternal
   use mapl3g_RestartModes, only: RestartMode, operator(==), MAPL_RESTART_SKIP
   use mapl3g_Field_API, only: MAPL_FieldGet
   use mapl3g_FieldBundle_API, only: MAPL_FieldBundleAdd, MAPL_FieldBundleGet
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
      procedure, private :: get_field_bundle_from_state_
      procedure, private :: filter_fields_
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
      bundle = this%get_field_bundle_from_state_(state, _RC)
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
      bundle = this%get_field_bundle_from_state_(state, _RC)
      bundle = this%filter_fields_(bundle, _RC)
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

      metadata = bundle_to_metadata(bundle, this%gridcomp_geom, _RC)
      allocate(writer, source=make_geom_pfio(metadata), _STAT)
      call writer%initialize(metadata, this%gridcomp_geom, _RC)
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
      integer :: status

      call file_formatter%open(filename, PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)
      allocate(reader, source=make_geom_pfio(metadata), _STAT)
      call reader%initialize(filename, this%gridcomp_geom, _RC)
      call reader%request_data_from_file(filename, bundle, _RC)
      call i_Clients%done_collective_prefetch()
      call i_Clients%wait()

      _RETURN(_SUCCESS)
   end subroutine read_bundle_

   recursive function get_field_bundle_from_state_(this, state, rc) result(bundle)
      class(RestartHandler), intent(in) :: this
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc
      type(ESMF_FieldBundle) :: bundle ! result

      ! character(len=:), allocatable :: prefix
      type(ESMF_Field) :: field, alias
      type(ESMF_Field), allocatable :: field_list(:)
      type(ESMF_FieldBundle) :: bundle2
      type (ESMF_StateItem_Flag), allocatable  :: item_types(:)
      character(len=ESMF_MAXSTR), allocatable :: item_names(:)
      character(len=:), allocatable :: item_name, short_name
      integer :: idx, jdx, item_count, status

      bundle = ESMF_FieldBundleCreate(_RC)
      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(item_names(item_count), _STAT)
      allocate(item_types(item_count), _STAT)
      call ESMF_StateGet(state, itemNameList=item_names, itemTypeList=item_types, _RC)
      do idx = 1, item_count
         if (allocated(field_list)) deallocate(field_list, _STAT)
         item_name = trim(item_names(idx))
         if (item_types(idx) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, item_name, field, _RC)
            call MAPL_FieldBundleAdd(bundle, [field], _RC)
         else if (item_types(idx) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, item_name, bundle2, _RC)
            call MAPL_FieldBundleGet(bundle2, fieldList=field_list, _RC)
            do jdx = 1, size(field_list)
               call MAPL_FieldGet(field_list(jdx), short_name=short_name, _RC)
               alias = ESMF_NamedAlias(field_list(jdx), name=item_name//"_"//short_name, _RC)
               call MAPL_FieldBundleAdd(bundle, [alias], _RC)
            end do
         else
            call this%lgr%warning("Item [ %a ] is not a field/bundle! Not handled", item_name)
         end if
      end do

      _RETURN(_SUCCESS)
   end function get_field_bundle_from_state_

   function filter_fields_(this, bundle_in, rc) result(filtered_bundle)
      class(RestartHandler), intent(in) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      integer, optional, intent(out) :: rc
      type(ESMF_FieldBundle) :: filtered_bundle ! result

      type(ESMF_Field), allocatable :: field_list(:)
      type(ESMF_Info) :: info
      type(RestartMode) :: restart_mode
      integer :: idx, alias_id, status

      filtered_bundle = ESMF_FieldBundleCreate(_RC)
      call MAPL_FieldBundleGet(bundle_in, fieldList=field_list, _RC)
      do idx = 1, size(field_list)
         call ESMF_InfoGetFromHost(field_list(idx), info, _RC)
         call ESMF_NamedAliasGet(field_list(idx), id=alias_id, _RC)
         call FieldInfoGetInternal(info, alias_id, restart_mode, _RC)
         if (restart_mode==MAPL_RESTART_SKIP) cycle
         call MAPL_FieldBundleAdd(filtered_bundle, [field_list(idx)], _RC)
      end do

      _RETURN(_SUCCESS)
   end function filter_fields_

end module mapl3g_RestartHandler

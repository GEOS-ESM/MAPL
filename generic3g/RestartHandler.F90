#include "MAPL.h"

module mapl3g_RestartHandler

   use, intrinsic :: iso_c_binding, only: c_ptr
   use esmf
   use mapl3g_Geom_API, only: MaplGeom
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use mapl3g_geomio, only: bundle_to_metadata, GeomPFIO, make_geom_pfio, get_mapl_geom
   use mapl3g_SharedIO, only: esmf_to_pfio_type
   use mapl3g_FieldBundle_API, only: MAPL_FieldBundleCreate
   use pFIO, only: PFIO_READ, FileMetaData, NetCDF4_FileFormatter
   use pFIO, only: i_Clients, o_Clients
   use pFlogger, only: logging, logger

   implicit none(type, external)
   private

   public :: RestartHandler
   public :: MAPL_RESTART
   public :: MAPL_RESTART_OPTIONAL
   public :: MAPL_RESTART_SKIP
   public :: MAPL_RESTART_REQUIRED
   public :: MAPL_RESTART_BOOT
   public :: MAPL_RESTART_SKIP_INITIAL

   type :: RestartHandler
      private
      type(ESMF_Geom) :: gc_geom
      type(ESMF_Time) :: currTime
      class(logger), pointer :: lgr => null()
   contains
      procedure, public :: write
      procedure, public :: read
      procedure, private :: write_bundle_
      procedure, private :: read_fields_
   end type RestartHandler

   interface RestartHandler
      procedure new_RestartHandler
   end interface RestartHandler

   enum, bind(C)
      enumerator :: MAPL_RESTART
      enumerator :: MAPL_RESTART_OPTIONAL
      enumerator :: MAPL_RESTART_SKIP
      enumerator :: MAPL_RESTART_REQUIRED
      enumerator :: MAPL_RESTART_BOOT
      enumerator :: MAPL_RESTART_SKIP_INITIAL
   end enum

contains

   function new_RestartHandler(gc_geom, currTime, gc_logger) result(restart_handler)
      type(RestartHandler) :: restart_handler ! result
      type(ESMF_Geom), intent(in) :: gc_geom
      type(ESMF_Time), intent(in) :: currTime
      class(logger), pointer, optional, intent(in) :: gc_logger

      integer :: status

      restart_handler%gc_geom = gc_geom
      restart_handler%currTime = currTime
      restart_handler%lgr => logging%get_logger('mapl.restart')
      if (present(gc_logger)) restart_handler%lgr => gc_logger

   end function new_RestartHandler

   subroutine write(this, state, filename, rc)
      ! Arguments
      class(RestartHandler), intent(inout) :: this
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: filename 
      integer, optional, intent(out) :: rc

      ! Locals
      type(ESMF_FieldBundle) :: out_bundle
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      if (item_count > 0) then
         call this%lgr%debug("Writing checkpoint: %a", filename)
         out_bundle = MAPL_FieldBundleCreate(state, _RC)
         call this%write_bundle_(out_bundle, filename, rc)
         call esmf_FieldBundleDestroy(out_bundle, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine write

   subroutine read(this, state, filename, rc)
      ! Arguments
      class(RestartHandler), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      ! Locals
      logical :: file_exists
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      if (item_count > 0) then
         inquire(file=filename, exist=file_exists)
         if (.not. file_exists) then
            ! TODO: Need to decide what happens in that case. Bootstrapping variables?
            call this%lgr%warning("Restart file << %a >> does not exist. Skip reading!", filename)
            _RETURN(_SUCCESS)
         end if
         call this%lgr%info("Reading restart: %a", trim(filename))
         call this%read_fields_(filename, state, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine read

   subroutine write_bundle_(this, bundle, filename, rc)
      ! Arguments
      class(RestartHandler), intent(in) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      ! Locals
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: writer
      type(MaplGeom), pointer :: mapl_geom
      integer :: status

      metadata = bundle_to_metadata(bundle, this%gc_geom, _RC)
      allocate(writer, source=make_geom_pfio(metadata), _STAT)
      mapl_geom => get_mapl_geom(this%gc_geom, _RC)
      call writer%initialize(metadata, mapl_geom, _RC)
      call writer%update_time_on_server(this%currTime, _RC)
      ! TODO: no-op if bundle is empty, or should we skip empty bundles?
      call writer%stage_data_to_file(bundle, filename, 1, _RC)
      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()
      deallocate(writer)

      _RETURN(_SUCCESS)
   end subroutine write_bundle_

   subroutine read_fields_(this, filename, state, rc)
      ! Arguments
      class(RestartHandler), intent(in) :: this
      character(len=*), intent(in) :: filename
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: reader
      type(MaplGeom), pointer :: mapl_geom
      integer :: status

      call file_formatter%open(filename, PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)
      allocate(reader, source=make_geom_pfio(metadata), _STAT)
      mapl_geom => get_mapl_geom(this%gc_geom, _RC)
      call reader%initialize(filename, mapl_geom, _RC)
      call reader%request_data_from_file(filename, state, _RC)
      call i_Clients%done_collective_prefetch()
      call i_Clients%wait()

      _RETURN(_SUCCESS)
   end subroutine read_fields_

end module mapl3g_RestartHandler

#include "MAPL_Generic.h"

module mapl3g_RestartHandler

   use, intrinsic :: iso_c_binding, only: c_ptr
   use esmf
   use mapl3g_Geom_API, only: MaplGeom
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use mapl3g_geomio, only: bundle_to_metadata, GeomPFIO, make_geom_pfio, get_mapl_geom
   use mapl3g_SharedIO, only: esmf_to_pfio_type
   use mapl3g_FieldBundleCreate, only: MAPL_FieldBundleCreate
   use pFIO, only: PFIO_READ, FileMetaData, NetCDF4_FileFormatter
   use pFIO, only: i_Clients, o_Clients
   use pFlogger, only: logging, logger

   implicit none
   private

   public :: RestartHandler

   type :: RestartHandler
      private
      character(len=ESMF_MAXSTR) :: gc_name
      type(ESMF_Geom) :: gc_geom
      type(ESMF_Time) :: current_time
      class(logger), pointer :: lgr
   contains
      procedure, public :: write
      procedure, public :: read
      procedure, private :: write_bundle_
      procedure, private :: read_fields_
   end type RestartHandler

   interface RestartHandler
      procedure new_RestartHandler
   end interface RestartHandler

contains

   function new_RestartHandler(gc_name, gc_geom, gc_clock, rc) result(restart_handler)
      character(len=*), intent(in) :: gc_name
      type(ESMF_Geom), intent(in) :: gc_geom
      type(ESMF_Clock), intent(in) :: gc_clock
      integer, optional, intent(out) :: rc
      type(RestartHandler) :: restart_handler ! result

      integer :: status

      restart_handler%gc_name = ESMF_UtilStringLowerCase(trim(gc_name), _RC)
      call ESMF_Clockget(gc_clock, currTime = restart_handler%current_time, _RC)
      restart_handler%gc_geom = gc_geom
      restart_handler%lgr => logging%get_logger('MAPL.GENERIC')

      _RETURN(_SUCCESS)
   end function new_RestartHandler

   subroutine write(this, state_type, state, rc)
      ! Arguments
      class(RestartHandler), intent(inout) :: this
      character(len=*), intent(in) :: state_type
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      type(ESMF_FieldBundle) :: out_bundle
      character(len=ESMF_MAXSTR) :: file_name
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      if (item_count > 0) then
         ! TODO: the file_name should come from OuterMetaComponents's hconfig
         file_name = trim(this%gc_name) // "_" // trim(state_type) // "_checkpoint.nc4"
         call this%lgr%info("Writing checkpoint: %a", trim(file_name))
         out_bundle = MAPL_FieldBundleCreate(state, _RC)
         call this%write_bundle_(out_bundle, file_name, rc)
      end if

      _RETURN(_SUCCESS)
   end subroutine write

   subroutine read(this, state_type, state, rc)
      ! Arguments
      class(RestartHandler), intent(inout) :: this
      character(len=*), intent(in) :: state_type
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      character(len=ESMF_MAXSTR) :: file_name
      logical :: file_exists
      integer :: item_count, status

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      if (item_count > 0) then
         ! TODO: the file_name should come from OuterMetaComponents's hconfig
         file_name = trim(this%gc_name) // "_" // trim(state_type) // "_rst.nc4"
         inquire(file=trim(file_name), exist=file_exists)
         if (.not. file_exists) then
            ! TODO: Need to decide what happens in that case. Bootstrapping variables?
            call this%lgr%info("Restart file << %a >> does not exist. Skip reading!", trim(file_name))
            _RETURN(_SUCCESS)
         end if
         call this%lgr%info("Reading restart: %a", trim(file_name))
         call this%read_fields_(file_name, state, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine read

   subroutine write_bundle_(this, bundle, file_name, rc)
      ! Arguments
      class(RestartHandler), intent(in) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: file_name
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
      call writer%update_time_on_server(this%current_time, _RC)
      ! TODO: no-op if bundle is empty, or should we skip empty bundles?
      call writer%stage_data_to_file(bundle, file_name, 1, _RC)
      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()
      deallocate(writer)

      _RETURN(_SUCCESS)
   end subroutine write_bundle_

   subroutine read_fields_(this, file_name, state, rc)
      ! Arguments
      class(RestartHandler), intent(in) :: this
      character(len=*), intent(in) :: file_name
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      ! Locals
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: reader
      type(MaplGeom), pointer :: mapl_geom
      integer :: status

      call file_formatter%open(file_name, PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)
      allocate(reader, source=make_geom_pfio(metadata), _STAT)
      mapl_geom => get_mapl_geom(this%gc_geom, _RC)
      call reader%initialize(file_name, mapl_geom, _RC)
      call reader%request_data_from_file(file_name, state, _RC)
      call i_Clients%done_collective_prefetch()
      call i_Clients%wait()

      _RETURN(_SUCCESS)
   end subroutine read_fields_

end module mapl3g_RestartHandler

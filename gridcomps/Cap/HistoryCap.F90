#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module HistoryCapMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use NUOPC_Model
   use MAPL_ExceptionHandling
   use MAPL_CapOptionsMod
   use MAPL_CapMod

   use FieldRegistryMod
   use NUOPCmapMod

   implicit none
   private

   public HistoryCap
   public i_set_services

   type :: HistoryCap
      private

      character(len=:), allocatable              :: name
      character(len=:), allocatable              :: rc_file
      procedure(i_set_services), nopass, pointer :: set_services
      type(MAPL_Cap), pointer                    :: cap => null()

      ! TODO: eliminate once we can use ESMF 8.1.0 specialize labels
      type(NUOPCmap), pointer                    :: phase_map => null()

      type(FieldRegistry) :: registry
      logical :: disable_throughput
   contains
      procedure :: initialize
      procedure :: init_cap
      procedure :: init_phase_map
      procedure :: init_mapl

      procedure :: init_p0
      procedure :: advertise
      procedure :: realize
      procedure :: data_init
      procedure :: advance
      procedure :: check_import
      procedure :: set_clock
      procedure :: finalize
   end type HistoryCap

   abstract interface
      subroutine i_set_services(gc, rc)
         import ESMF_GridComp
         type(ESMF_GridComp), intent(inout) :: gc
         integer,             intent(  out) :: rc
      end subroutine i_set_services
   end interface
contains
   subroutine initialize(this, name, root_rc, set_services, registry, disable_throughput)
      class(HistoryCap),                  intent(  out) :: this
      character(*),                       intent(in   ) :: name
      character(*),                       intent(in   ) :: root_rc
      procedure(i_set_services), pointer, intent(in   ) :: set_services
      type(FieldRegistry),                intent(in   ) :: registry
      logical,                            intent(in   ) :: disable_throughput

      this%name         =  name
      this%rc_file      =  root_rc
      this%set_services => set_services
      this%registry     =  registry
      this%disable_throughput = disable_throughput
   end subroutine initialize

   subroutine init_cap(this, model, rc)
      class(HistoryCap),   intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: model
      integer, optional,   intent(  out) :: rc

      integer                 :: mpi_comm, dup_comm
      type(ESMF_VM)           :: vm
      type(MAPL_CapOptions)   :: cap_options
      type(MAPL_Cap), pointer :: cap

      integer :: status

      ! Read ESMF VM information
      call ESMF_GridCompGet(model, vm=vm, __RC__)
      call ESMF_VMGet(vm, mpiCommunicator=mpi_comm, __RC__)
      call MPI_Comm_dup(mpi_comm, dup_comm, status)
      _VERIFY(status)

      cap_options                = MAPL_CapOptions(cap_rc_file=this%rc_file, __RC__)
      cap_options%use_comm_world = .false.
      cap_options%comm           = dup_comm

      ! TODO: this may need to be updated
      cap_options%logging_config = ''

      call MPI_Comm_size(dup_comm, cap_options%npes_model, status)
      _VERIFY(status)

      allocate(cap)
      cap = MAPL_Cap(this%name, this%set_services, cap_options=cap_options, __RC__)
      this%cap => cap

      _RETURN(_SUCCESS)
   end subroutine init_cap

   subroutine init_phase_map(this, model, rc)
      class(HistoryCap),   intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: model
      integer, optional,   intent(  out) :: rc

      type(NUOPCmap), pointer :: phase_map

      integer :: status

      allocate(phase_map)
      call phase_map%create_phase_map(model, __RC__)
      this%phase_map => phase_map

      _RETURN(_SUCCESS)
   end subroutine init_phase_map

   subroutine init_mapl(this, rc)
      class(HistoryCap), intent(inout) :: this
      integer, optional, intent(  out) :: rc

      integer :: status

      ! Create/initialize the Cap GridComp
      call this%cap%initialize_io_clients_servers(this%cap%get_comm_world(), __RC__)
      call this%cap%initialize_cap_gc(export_field_registry=this%registry,disable_throughput=this%disable_throughput,__RC__)

      ! Call MAPL set_services and initialize MAPL components
      call this%cap%cap_gc%set_services(__RC__)
      call this%cap%cap_gc%initialize(__RC__)

      _RETURN(_SUCCESS)
   end subroutine init_mapl

   subroutine init_p0(this, model, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      !type(ESMF_State)                  :: import_state
      !type(ESMF_State)                  :: export_state
      !type(ESMF_Clock)                  :: clock
      integer,            intent(  out) :: rc

      integer :: status

      rc = ESMF_SUCCESS

      call this%init_cap(model, __RC__)
      call this%init_mapl(__RC__)

   end subroutine init_p0

   subroutine advertise(this, model, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state

      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(model, importState=import_state, exportState=export_state, rc=rc)
      VERIFY_NUOPC_(rc)

      ! Advertise the GEOS fields as exports for History to receive
      call this%registry%advertise(export_state, rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine advertise

   subroutine realize(this, model, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state

      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(model, importState=import_state, exportState=export_state, rc=rc)
      VERIFY_NUOPC_(rc)

      ! Realize the GEOS fields as exports for History
      call this%registry%realize(export_state, this%cap%cap_gc%export_state, rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine realize

   subroutine data_init(this, model, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      rc = ESMF_SUCCESS

      call NUOPC_CompAttributeSet(model, &
         name="InitializeDataComplete", value="true", rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine data_init

   subroutine advance(this, rc)
      class(HistoryCap), intent(inout) :: this
      integer,           intent(  out) :: rc

      rc = ESMF_SUCCESS

      call this%cap%step_model(rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine advance

   subroutine check_import(this, rc)
      class(HistoryCap), intent(inout) :: this
      integer,           intent(  out) :: rc

      rc = ESMF_SUCCESS
   end subroutine check_import

   subroutine set_clock(this, model, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_TimeInterval) :: time_step
      type(ESMF_Clock)        :: model_clock
      integer                 :: heartbeat_dt

      rc = ESMF_SUCCESS

      ! set time interval
      heartbeat_dt = this%cap%cap_gc%get_heartbeat_dt()
      call ESMF_TimeIntervalSet(time_step, s=heartbeat_dt, rc=rc)
      VERIFY_NUOPC_(rc)

      ! set clock with time interval
      call NUOPC_ModelGet(model, modelClock=model_clock, rc=rc)
      VERIFY_NUOPC_(rc)
      call ESMF_ClockSet(model_clock, timeStep=time_step, rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine set_clock

   subroutine finalize(this, rc)
      class(HistoryCap), intent(inout) :: this
      integer,           intent(  out) :: rc

      call this%cap%cap_gc%finalize(rc=rc)
      VERIFY_NUOPC_(rc)
      rc = ESMF_SUCCESS
   end subroutine finalize
end module HistoryCapMod

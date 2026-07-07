#include "MAPL.h"

! The derived type "MaplFramework" is intended to encapsulate all of the singletons used within MAPL-based
! codes.   This limits the scope of the singleton "sin", which will allow proper object passing
! at some later date if justified.


module mapl_MaplFramework_mod

   use mapl_ErrorHandling_mod
   use mapl_MaplServerUtilities_mod
   use mapl_MpiErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use mapl_FieldFillDefault_mod, only: &
        field_fill_defaults_init => initialize_field_fill_defaults, &
        set_field_fill_defaults
   use mapl_vertical_grid_api
   ! Note: mapl_VerticalGridManager_mod used inside initialize() only
   use mapl_FixedLevelsVerticalGrid_mod
   use mapl_ModelVerticalGrid_mod
   use mapl_FieldDictionary_mod, only: load_field_dictionary
   use mapl_Profiler_mod, only: profiler_initialize => initialize, profiler_finalize => finalize
   use pfio_DirectoryServiceMod, only: DirectoryService
   use pfio_ClientManagerMod
   use pfio_MpiServerMod, only: MpiServer
   use pfio_AbstractDirectoryServiceMod, only: PortInfo
   use udunits2f, only: UDUNITS_Initialize => Initialize
   use pflogger, only: logging
   use pflogger, only: Logger
   use mpi
   use esmf

   implicit none
   private

   public :: MaplFramework
   public :: MAPL_initialize
   public :: MAPL_finalize
   public :: MAPL_Get
   public :: MAPL_CreateServers
   public :: MAPL_RunServers

   type :: MaplFramework
      private
      logical :: mapl_initialized = .false.
      logical :: esmf_internally_initialized = .false.
      type(ESMF_VM) :: mapl_vm
      integer :: model_comm

      type(ESMF_HConfig) :: hconfig       ! full top-level hconfig
      type(ESMF_HConfig) :: mapl_hconfig  ! mapl: subsection
      type(DirectoryService) :: directory_service
      type(MpiServer), pointer :: o_server => null()
      type(MpiServer), pointer :: i_server => null()
      logical :: is_model_pet = .false.
   contains
      procedure :: initialize
      procedure :: initialize_esmf
#ifdef BUILD_WITH_PFLOGGER
      procedure :: initialize_pflogger
#endif
      procedure :: initialize_profilers
      procedure :: initialize_udunits
      procedure :: get_vm_topology
      procedure :: validate_resources
      procedure :: create_servers
      procedure :: initialize_simple_servers
      procedure :: initialize_complex_servers
      procedure :: run_servers
      procedure :: initialize_field_dictionary
      procedure :: initialize_field_fill_defaults

      procedure :: finalize
      procedure :: finalize_servers
      procedure :: finalize_profiler
      procedure :: finalize_pflogger
      procedure :: finalize_esmf
      procedure :: get
      procedure :: is_initialized
   end type MaplFramework

   ! Private singleton object.  Used
   type(MaplFramework), target :: the_mapl_object

   interface MAPL_Get
      procedure :: mapl_get
      procedure :: mapl_get_mapl
   end interface MAPL_Get

   interface MAPL_Initialize
      procedure :: mapl_initialize
   end interface MAPL_Initialize

   interface MAPL_CreateServers
      procedure :: mapl_create_servers
   end interface MAPL_CreateServers

   interface MAPL_RunServers
      procedure :: mapl_run_servers
   end interface MAPL_RunServers

contains

   ! Type-bound procedures

   ! Note: hconfig (path b) is intent(in) — ESMF is already initialized by caller.
   !       configFilenameFromArgNum (path a) — MAPL initializes ESMF internally.
   subroutine initialize(this, hconfig, unusable, mpiCommunicator, level_name, configFilenameFromArgNum, &
        field_default_fill_value_r4, field_default_fill_value_r8, rc)
      class(MaplFramework), intent(inout) :: this
      type(ESMF_HConfig), optional, intent(in) :: hconfig  ! path (b): already-initialized ESMF
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mpiCommunicator
      character(*), optional, intent(in) :: level_name
      integer, optional, intent(in) :: configFilenameFromArgNum
      real(ESMF_KIND_R4), optional, intent(in) :: field_default_fill_value_r4
      real(ESMF_KIND_R8), optional, intent(in) :: field_default_fill_value_r8
      integer, optional, intent(out) :: rc
      type(mapl_VerticalGridManager), pointer :: vgrid_manager

      integer :: status
      type(FixedLevelsVerticalGridFactory) :: fixed_levels_vgrid_factory
      type(ModelVerticalGridFactory) :: model_vgrid_factory


      _ASSERT(.not. this%mapl_initialized, "MaplFramework object is already initialized")
      this%mapl_initialized = .true.

      call this%initialize_esmf(hconfig=hconfig, mpiCommunicator=mpiCommunicator, configFilenameFromArgNum=configFilenameFromArgNum, _RC)
      call ESMF_VMGetCurrent(this%mapl_vm, _RC)

#ifdef BUILD_WITH_PFLOGGER
      call this%initialize_pflogger(level_name=level_name, _RC)
#endif
      call this%initialize_profilers(_RC)
      call this%validate_resources(_RC)
      call this%initialize_udunits(_RC)
      call this%initialize_field_dictionary(_RC)
      call this%initialize_field_fill_defaults( &
           field_default_fill_value_r4=field_default_fill_value_r4, &
           field_default_fill_value_r8=field_default_fill_value_r8, &
           _RC)

      vgrid_manager => mapl_get_vertical_grid_manager(_RC)
      call vgrid_manager%initialize(_RC)
      call vgrid_manager%register_factory("FixedLevels", fixed_levels_vgrid_factory, _RC)
      call vgrid_manager%register_factory("Model", model_vgrid_factory, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize

   ! Path (a) — standalone: MAPL calls ESMF_Initialize, derives hconfig from YAML file.
   ! Path (b) — embedded: ESMF already initialized; hconfig passed in by caller.
   ! In both cases this%hconfig holds the full top-level config and
   ! this%mapl_hconfig holds the mapl: subsection (or an empty map).
   subroutine initialize_esmf(this, hconfig, unusable, mpiCommunicator, configFilenameFromArgNum, rc)
      class(MaplFramework), intent(inout) :: this
      type(ESMF_HConfig), optional, intent(in) :: hconfig  ! path (b) only
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(in) :: configFilenameFromArgNum
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Config) :: config
      logical :: esmf_is_initialized
      integer :: argNum


      esmf_is_initialized = ESMF_IsInitialized(_RC)

      if (esmf_is_initialized) then
         ! Path (b): embedded / library mode — ESMF already initialized by caller.
         _ASSERT(present(hconfig), "hconfig must be provided when ESMF is already initialized (path b)")
         this%hconfig = hconfig
         this%mapl_hconfig = get_subconfig(this%hconfig, keystring='mapl', _RC)
         _RETURN(_SUCCESS)
      end if

      ! Path (a): standalone — MAPL initializes ESMF, derives hconfig from YAML config file.
      this%esmf_internally_initialized = .true.

      argNum = 0
      if (present(configFilenameFromArgNum)) argNum = configFilenameFromArgNum

      if (argNum > 0) then
         call ESMF_Initialize(configFilenameFromArgNum=argNum, configKey=['esmf'], config=config, &
              defaultDefaultCalKind=ESMF_CALKIND_GREGORIAN, &
              mpiCommunicator=mpiCommunicator, _RC)
         call ESMF_ConfigGet(config, hconfig=this%hconfig, _RC)
         this%mapl_hconfig = get_subconfig(this%hconfig, keystring='mapl', _RC)
      else
         call ESMF_Initialize(mpiCommunicator=mpiCommunicator, defaultDefaultCalKind=ESMF_CALKIND_GREGORIAN, _RC)
         this%hconfig = ESMF_HConfigCreate(content='{}', _RC)
         this%mapl_hconfig = ESMF_HConfigCreate(content='{}', _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   contains

      ! Return an empty mapping unless named sub-section is found.
      function get_subconfig(hconfig, keystring, rc) result(subcfg)
         type(ESMF_HConfig) :: subcfg
         type(ESMF_HConfig), intent(in) :: hconfig
         character(*), intent(in) :: keystring
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_keystring

         has_keystring = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
         if (has_keystring) then
            subcfg = ESMF_HConfigCreateAt(hconfig, keystring=keystring, _RC)
            _RETURN(_SUCCESS)
         end if

         subcfg = ESMF_HConfigCreate(content='{}', _RC)
         _RETURN(_SUCCESS)
      end function get_subconfig

   end subroutine initialize_esmf

#ifdef BUILD_WITH_PFLOGGER
   subroutine initialize_pflogger(this, unusable, level_name, rc)
      use PFL_Formatter, only: get_sim_time
      use pflogger, only: pfl_initialize => initialize
      use mapl_SimulationTime_mod, only: fill_time_dict

      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: level_name
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: world_comm
      logical :: has_pflogger_cfg_file
      character(:), allocatable :: pflogger_cfg_file

      call pfl_initialize()
      get_sim_time => fill_time_dict

      has_pflogger_cfg_file = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring="pflogger_cfg_file", _RC)
      if (has_pflogger_cfg_file) then
         pflogger_cfg_file = ESMF_HConfigAsString(this%mapl_hconfig, keystring="pflogger_cfg_file", _RC)
         call logging%load_file(pflogger_cfg_file)
         _RETURN(_SUCCESS)
      end if

      call ESMF_VMGet(this%mapl_vm, mpiCommunicator=world_comm, _RC)
      call default_initialize_pflogger(world_comm=world_comm, level_name=level_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_pflogger
#endif


   subroutine initialize_profilers(this, rc)
      class(MaplFramework), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: world_comm
      integer :: status

      call ESMF_VMGet(this%mapl_vm, mpiCommunicator=world_comm, _RC)
      call profiler_initialize(comm=world_comm, enable_global_timeprof=.true., enable_global_memprof=.true., _RC)

      _RETURN(_SUCCESS)
      ! _UNUSED_DUMMY(unusable)
   end subroutine initialize_profilers

   ! Query the VM for the SSI map and apply the pet_as_ssi testing override.
   ! pet_as_ssi: true in the MAPL hconfig treats every PET as its own SSI,
   ! allowing multi-server partitioning logic to run on a single physical node.
   subroutine get_vm_topology(this, ssiMap, ssiCount, world_comm, rc)
      class(MaplFramework), intent(in) :: this
      integer, allocatable, intent(out) :: ssiMap(:)
      integer, intent(out) :: ssiCount
      integer, intent(out) :: world_comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: n
      logical :: pet_as_ssi
      ! petCount is required by the ESMF_VMGet interface but is not used here.
      ! The dummy argument works around an ESMF v8.6.0 bug where omitting it
      ! causes incorrect results for the other outputs.
      integer :: petCount_unused

      call ESMF_VMGet(this%mapl_vm, ssiMap=ssiMap, ssiCount=ssiCount, &
           mpiCommunicator=world_comm, petCount=petCount_unused, _RC)

      pet_as_ssi = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='pet_as_ssi', _RC)
      if (pet_as_ssi) pet_as_ssi = ESMF_HConfigAsLogical(this%mapl_hconfig, keystring='pet_as_ssi', _RC)
      if (pet_as_ssi) then
         ssiCount = size(ssiMap)
         ssiMap = [(n, n = 0, ssiCount - 1)]
      end if

      _RETURN(_SUCCESS)
   end subroutine get_vm_topology

   ! Fast-fail resource validation: check that model + server SSI allocations
   ! do not exceed available SSIs.  Runs in MAPL_Initialize before any
   ! collective server/cap creation so errors surface immediately.
   subroutine validate_resources(this, unusable, rc)
      class(MaplFramework), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_server_section
      integer :: model_petCount
      integer :: world_comm
      integer :: ssiCount
      integer, allocatable :: ssiMap(:)
      integer :: num_model_ssis, required_ssis
      type(ESMF_HConfig) :: servers_hconfig
      type(ESMF_HConfig), allocatable :: server_hconfigs(:)
      integer, allocatable :: ssis_per_server(:)
      class(Logger), pointer :: lgr

      has_server_section = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='servers', _RC)
      _RETURN_UNLESS(has_server_section)

      call this%get_vm_topology(ssiMap=ssiMap, ssiCount=ssiCount, world_comm=world_comm, _RC)
      model_petCount = get_model_petcount(this%mapl_vm, this%mapl_hconfig, _RC)
      num_model_ssis = get_num_ssis(model_petCount, ssiMap, ssiOffset=0, _RC)

      servers_hconfig = ESMF_HConfigCreateAt(this%mapl_hconfig, keystring='servers', _RC)
      server_hconfigs = get_server_hconfigs(servers_hconfig, _RC)
      ssis_per_server = get_ssis_per_server(server_hconfigs, ssiCount=ssiCount, num_model_ssis=num_model_ssis, _RC)

      required_ssis = num_model_ssis + sum(ssis_per_server)
      _ASSERT(required_ssis <= ssiCount, "Insufficient resources: PET allocations exceed available SSIs.")
      if (required_ssis < ssiCount) then
         lgr => logging%get_logger('MAPL')
         call lgr%warning("Unused nodes.  Required %i0 nodes, but %i0 available.", required_ssis, ssiCount)
      end if

      call ESMF_HConfigDestroy(servers_hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine validate_resources

   ! Create server communicators and ESMF GridComps for any explicit servers:
   ! section, or set up in-process simple servers if no servers: section exists.
   ! Sets this%is_model_pet and returns server GridComps via servers(:).
   subroutine create_servers(this, servers, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      type(ESMF_GridComp), allocatable, intent(out) :: servers(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_server_section
      integer :: model_petCount
      integer :: world_group, model_group
      integer :: world_comm
      integer :: ssiCount
      integer, allocatable :: ssiMap(:)

      call this%get_vm_topology(ssiMap=ssiMap, ssiCount=ssiCount, world_comm=world_comm, _RC)
      model_petCount = get_model_petcount(this%mapl_vm, this%mapl_hconfig, _RC)

      has_server_section = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='servers', _RC)
      if (.not. has_server_section) then
         ! Simple path: in-process servers; no ESMF GridComp servers returned.
         allocate(servers(0))
         call MPI_Comm_group(world_comm, world_group, _IERROR)
         call MPI_Group_range_incl(world_group, 1, reshape([0, model_petCount-1, 1], [3,1]), model_group, _IERROR)
         call MPI_Comm_create_group(world_comm, model_group, 0, this%model_comm, _IERROR)
         call MPI_Group_free(model_group, _IERROR)
         call MPI_Group_free(world_group, _IERROR)
         this%is_model_pet = (this%model_comm /= MPI_COMM_NULL)
         _RETURN_UNLESS(this%is_model_pet)
         this%directory_service = DirectoryService(this%model_comm)
         call this%initialize_simple_servers(_RC)
         _RETURN(_SUCCESS)
      end if

      ! Complex path: explicit server topology from servers: section.
      call this%initialize_complex_servers(servers, world_comm, model_petCount, ssiCount, ssiMap, &
           is_model_pet=this%is_model_pet, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine create_servers

   ! Build MPI communicators and ESMF GridComps for an explicit servers: topology.
   subroutine initialize_complex_servers(this, servers, world_comm, model_petCount, ssiCount, ssiMap, &
        unusable, is_model_pet, rc)
      class(MaplFramework), target, intent(inout) :: this
      type(ESMF_GridComp), allocatable, intent(out) :: servers(:)
      integer, intent(in) :: world_comm
      integer, intent(in) :: model_petCount
      integer, intent(in) :: ssiCount
      integer, intent(in) :: ssiMap(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_model_pet
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: servers_hconfig
      integer :: world_group, model_group
      integer :: server_comm, model_server_comm
      integer :: num_model_ssis
      integer :: ssi_0, ssi_1, i_server
      integer, allocatable :: ssis_per_server(:)
      integer, allocatable :: model_pets(:), server_pets(:), model_server_pets(:)
      type(ESMF_HConfig), allocatable :: server_hconfigs(:)
      class(Logger), pointer :: lgr

      num_model_ssis = get_num_ssis(model_petCount, ssiMap, ssiOffset=0, _RC)

      servers_hconfig = ESMF_HConfigCreateAt(this%mapl_hconfig, keystring='servers', _RC)
      server_hconfigs = get_server_hconfigs(servers_hconfig, _RC)

      ! get_ssis_per_server handles '*' wildcard for the last server.
      ! Resource validation already ran in MAPL_Initialize; this is belt-and-suspenders.
      ssis_per_server = get_ssis_per_server(server_hconfigs, ssiCount=ssiCount, num_model_ssis=num_model_ssis, _RC)

      call MPI_Comm_group(world_comm, world_group, _IERROR)
      model_pets = pets_on_ssis(ssiMap, 0, num_model_ssis)
      call MPI_Group_incl(world_group, size(model_pets), model_pets, model_group, _IERROR)
      call MPI_Comm_create_group(world_comm, model_group, 0, this%model_comm, _IERROR)
      if (present(is_model_pet)) is_model_pet = (this%model_comm /= MPI_COMM_NULL)

      ssi_0 = num_model_ssis
      allocate(servers(size(server_hconfigs)))
      do i_server = 1, size(server_hconfigs)
         ssi_1 = ssi_0 + ssis_per_server(i_server)
         server_pets = pets_on_ssis(ssiMap, ssi_0, ssi_1)
         call create_server_comms(world_comm, world_group, model_group, server_pets, server_comm, model_server_comm, _RC)
         model_server_pets = [model_pets, server_pets]
         servers(i_server) = make_server_gridcomp(server_hconfigs(i_server), &
              model_server_pets, [model_server_comm, this%model_comm, server_comm], _RC)
         ssi_0 = ssi_1
      end do

      call MPI_Group_Free(model_group, _IERROR)
      call MPI_Group_Free(world_group, _IERROR)
      call ESMF_HConfigDestroy(servers_hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_complex_servers

   subroutine initialize_simple_servers(this, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, stat_alloc

      call init_IO_ClientManager(this%model_comm, _RC)

      ! o server
      allocate(this%o_server, source=MpiServer(this%model_comm, 'o_server', rc=status), stat=stat_alloc)
      _VERIFY(status)
      _VERIFY(stat_alloc)
      call this%directory_service%publish(PortInfo('o_server', this%o_server), this%o_server)
      call this%directory_service%connect_to_server('o_server', o_Client)

      ! i server
      allocate(this%i_server, source=MpiServer(this%model_comm, 'i_server', rc=status), stat=stat_alloc)
      _VERIFY(status)
      _VERIFY(stat_alloc)
      call this%directory_service%publish(PortInfo('i_server', this%i_server), this%i_server)
      call this%directory_service%connect_to_server('i_server', i_Client)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_simple_servers

   ! Run servers on server PETs; model PETs return immediately.
   ! ESMF_GridCompInitialize/Run/Finalize only require the petList PETs —
   ! no global collective needed.
   subroutine run_servers(this, servers, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: servers(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i, status

      ! Model PETs have nothing to do here.
      _RETURN_IF(this%is_model_pet)

      ! Server PETs run the lifecycle for each server GridComp.
      ! ESMF only executes on PETs in the GridComp's petList; other
      ! server PETs silently skip GridComps they don't belong to.
      do i = 1, size(servers)
         call ESMF_GridCompInitialize(servers(i), _RC)
         call ESMF_GridCompRun(servers(i), _RC)
         call ESMF_GridCompFinalize(servers(i), _RC)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_servers

   subroutine get(this, unusable, directory_service, is_model_pet, hconfig, rc)
      class(MaplFramework), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      logical, optional, intent(out) :: is_model_pet
      type(ESMF_HConfig), optional, intent(out) :: hconfig
      integer, optional, intent(out) :: rc

      _ASSERT(this%is_initialized(), "MaplFramework object is not initialized")
      if (present(directory_service)) directory_service => this%directory_service
      if (present(is_model_pet)) is_model_pet = this%is_model_pet
      if (present(hconfig)) hconfig = this%hconfig

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine get

   logical function is_initialized(this)
      class(MaplFramework), intent(in) :: this
      is_initialized = this%mapl_initialized
   end function is_initialized

   subroutine finalize(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%model_comm /= MPI_COMM_NULL) then
         call this%directory_service%free_directory_resources()
         call MPI_Comm_free(this%model_comm, _IERROR)
      end if
      call this%finalize_servers(_RC)
      !#         call server_comm%free_comms(_RC)
      !#         if (server_comm /= MPI_COMM_NULL) then
      !#            call MPI_Comm_free(server_comm, _IERROR)
      !#         end if
      !#         if (server_comm_model /= MPI_COMM_NULL) then
      !#            call MPI_Comm_free(server_comm_model, _IERROR)
      !#         end if

      call this%finalize_profiler(_RC)
      call this%finalize_pflogger(_RC)
      call this%finalize_esmf(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize

   subroutine finalize_servers(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(this)
   end subroutine finalize_servers

   subroutine finalize_profiler(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call profiler_finalize(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(this)
   end subroutine finalize_profiler

   subroutine finalize_pflogger(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call logging%free()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(this)
   end subroutine finalize_pflogger

   subroutine finalize_esmf(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(this%esmf_internally_initialized)

      call ESMF_HConfigDestroy(this%mapl_hconfig, _RC)
      call ESMF_HConfigDestroy(this%hconfig, _RC)
      call ESMF_Finalize(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize_esmf

   ! Public interfaces that rely on the singleton object
   subroutine mapl_get(unusable, directory_service, is_model_pet, hconfig, rc)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      logical, optional, intent(out) :: is_model_pet
      type(ESMF_HConfig), optional, intent(out) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%get(directory_service=directory_service, &
           is_model_pet=is_model_pet, hconfig=hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_get

   subroutine mapl_get_mapl(mapl)
      type(MaplFramework), pointer, intent(out) :: mapl

      mapl => the_mapl_object
   end subroutine mapl_get_mapl


   subroutine mapl_initialize(hconfig, unusable, mpiCommunicator, configFilenameFromArgNum, level_name, &
        field_default_fill_value_r4, field_default_fill_value_r8, rc)
      type(ESMF_HConfig), optional, intent(in) :: hconfig  ! path (b): already-initialized ESMF
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(in) :: configFilenameFromArgNum
      character(*), optional, intent(in) :: level_name
      real(ESMF_KIND_R4), optional, intent(in) :: field_default_fill_value_r4
      real(ESMF_KIND_R8), optional, intent(in) :: field_default_fill_value_r8
      integer, optional, intent(out) :: rc

      integer :: status

      call mapl_initialize_error_handling()

      call the_mapl_object%initialize(hconfig=hconfig, mpiCommunicator=mpiCommunicator, &
           configFilenameFromArgNum=configFilenameFromArgNum, level_name=level_name, &
           field_default_fill_value_r4=field_default_fill_value_r4, &
           field_default_fill_value_r8=field_default_fill_value_r8, &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_initialize

   subroutine mapl_finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_finalize

   subroutine mapl_create_servers(servers, unusable, rc)
      type(ESMF_GridComp), allocatable, intent(out) :: servers(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%create_servers(servers, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_create_servers

   subroutine mapl_run_servers(servers, unusable, rc)
      type(ESMF_GridComp), intent(inout) :: servers(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%run_servers(servers, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_run_servers

#ifdef BUILD_WITH_PFLOGGER
   subroutine default_initialize_pflogger(world_comm, unusable, level_name, rc)
      use pflogger, only: StreamHandler, FileHandler, HandlerVector
      use pflogger, only: MpiLock, MpiFormatter
      use pflogger, only: INFO, WARNING, name_to_level

      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT

      integer, intent(in) :: world_comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: level_name
      integer, optional, intent(out) :: rc

      type (HandlerVector) :: handlers
      type (StreamHandler) :: console
      type (FileHandler) :: file_handler
      integer :: level,rank,status
      type(Logger), pointer :: lgr
      character(:), allocatable :: level_name_

      ! Default configuration if no file provided

      level_name_ = 'INFO'
      if (present(level_name)) level_name_ = level_name

      call MPI_COMM_Rank(world_comm,rank,status)
      level = WARNING ! except on root
      if (rank == 0) then
         level = name_to_level(level_name_)
      end if

      console = StreamHandler(OUTPUT_UNIT)
      call console%set_level(level)
      call console%set_formatter(MpiFormatter(world_comm, fmt='%(name)a15~: %(message)a'))
      call handlers%push_back(console)

      file_handler = FileHandler('warnings_and_errors.log')
      call file_handler%set_level(WARNING)
      call file_handler%set_formatter(MpiFormatter(world_comm, fmt='pe=%(mpi_rank)i5.5~: %(name)a~: %(message)a'))
      call file_handler%set_lock(MpiLock(world_comm))
      call handlers%push_back(file_handler)

      call logging%basic_config(level=level, handlers=handlers, rc=status)
      _VERIFY(status)

      if (rank == 0) then
         lgr => logging%get_logger('mapl')
         call lgr%info('No configure file specified for logging layer.  Using defaults.')
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine default_initialize_pflogger
#endif

   subroutine initialize_field_fill_defaults(this, unusable, field_default_fill_value_r4, field_default_fill_value_r8, rc)
      class(MaplFramework), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(ESMF_KIND_R4), optional, intent(in) :: field_default_fill_value_r4
      real(ESMF_KIND_R8), optional, intent(in) :: field_default_fill_value_r8
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_generic, has_r4, has_r8
      real(ESMF_KIND_R4), allocatable :: fill_value_from_yaml_r4
      real(ESMF_KIND_R8), allocatable :: fill_value_from_yaml_r8

      ! Set module singleton defaults to sNaN before applying any overrides.
      call field_fill_defaults_init()

      has_generic = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='field_default_fill_value', _RC)
      has_r4 = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='field_default_fill_value_r4', _RC)
      has_r8 = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='field_default_fill_value_r8', _RC)

      ! Disallow simultaneous use of generic and specific YAML keys
      _ASSERT(.not. (has_generic .and. has_r4), "'field_default_fill_value' and 'field_default_fill_value_r4' cannot both be set in YAML config")
      _ASSERT(.not. (has_generic .and. has_r8), "'field_default_fill_value' and 'field_default_fill_value_r8' cannot both be set in YAML config")

      ! Disallow simultaneous Fortran argument and YAML key for the same typekind
      _ASSERT(.not. (present(field_default_fill_value_r4) .and. (has_r4 .or. has_generic)), "field_default_fill_value_r4 specified both as Fortran argument and in YAML config")
      _ASSERT(.not. (present(field_default_fill_value_r8) .and. (has_r8 .or. has_generic)), "field_default_fill_value_r8 specified both as Fortran argument and in YAML config")

      ! Apply Fortran arguments (if present)
      if (present(field_default_fill_value_r4)) allocate(fill_value_from_yaml_r4, source=field_default_fill_value_r4)
      if (present(field_default_fill_value_r8)) allocate(fill_value_from_yaml_r8, source=field_default_fill_value_r8)

      ! Apply YAML generic key (sets both R4 and R8)
      if (has_generic) then
         fill_value_from_yaml_r4 = ESMF_HConfigAsR4(this%mapl_hconfig, keystring='field_default_fill_value', _RC)
         fill_value_from_yaml_r8 = ESMF_HConfigAsR8(this%mapl_hconfig, keystring='field_default_fill_value', _RC)
      end if

      ! Apply YAML typekind-specific keys
      if (has_r4) fill_value_from_yaml_r4 = &
           ESMF_HConfigAsR4(this%mapl_hconfig, keystring='field_default_fill_value_r4', _RC)
      if (has_r8) fill_value_from_yaml_r8 = &
           ESMF_HConfigAsR8(this%mapl_hconfig, keystring='field_default_fill_value_r8', _RC)

      call set_field_fill_defaults(r4=fill_value_from_yaml_r4, r8=fill_value_from_yaml_r8)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_field_fill_defaults

   subroutine initialize_udunits(this, rc)
      class(MaplFramework), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call UDUNITS_Initialize(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine initialize_udunits

   subroutine initialize_field_dictionary(this, rc)
      class(MaplFramework), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_path, file_exists
      character(:), allocatable :: path
      type(Logger), pointer :: lgr

      has_path = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='field_dictionary', _RC)
      if (has_path) then
         path = ESMF_HConfigAsString(this%mapl_hconfig, keystring='field_dictionary', _RC)
      else
         path = 'geos_field_dictionary.yaml'
      end if

      inquire(file=path, exist=file_exists)
      if (file_exists) then
         call load_field_dictionary(path, _RC)
      else if (has_path) then
         ! Explicitly configured path must exist — fail hard.
         _ASSERT(.false., 'Field dictionary not found at configured path: "'//path//'"')
      else
         ! Default path absent — warn and proceed without the dictionary.
         lgr => logging%get_logger('MAPL')
         call lgr%warning('Field dictionary not loaded: "'//path//'" not found. ' // &
              'Dictionary defaults (units, long_name) will not be applied.')
      end if

      _RETURN(_SUCCESS)
   end subroutine initialize_field_dictionary

end module mapl_MaplFramework_mod

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
    use mapl_DefaultServerNames_mod, only: MAPL_DEFAULT_INPUT_SERVER, MAPL_DEFAULT_OUTPUT_SERVER
     use pfio_DirectoryServiceMod, only: DirectoryService
     use pfio_ClientManagerMod, only: get_client, add_client
    use pfio_MpiServerMod, only: MpiServer
    use pfio_MultiGroupServerMod, only: MultiGroupServer
    use pfio_BaseServerMod, only: BaseServer
    use pfio_StringServerMapMod, only: StringServerMap
    use pfio_ClientThreadMod, only: ClientThread
    use pfio_FastClientThreadMod, only: FastClientThread
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
   public :: MAPL_ConnectToServer
   public :: MAPL_PublishServer

   type :: MaplFramework
      private
      logical :: mapl_initialized = .false.
      logical :: esmf_internally_initialized = .false.
      type(ESMF_VM) :: mapl_vm
      integer :: model_comm

      type(ESMF_HConfig) :: hconfig       ! full top-level hconfig
      type(ESMF_HConfig) :: mapl_hconfig  ! mapl: subsection
      type(DirectoryService) :: directory_service
      type(StringServerMap) :: local_server_map
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
      procedure :: initialize_default_servers
      procedure :: create_servers
      procedure :: initialize_configured_local_servers
      procedure :: add_local_server
      procedure :: initialize_non_default_servers
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

   ! Names of clients connected via MAPL_ConnectToServer (external servers).
   ! Model PETs must call terminate() on each before ESMF_Finalize.
   integer, save :: n_ext_clients = 0
   character(ESMF_MAXSTR), save :: ext_client_names(64)

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

   interface MAPL_ConnectToServer
      procedure :: mapl_connect_to_server
   end interface MAPL_ConnectToServer

   interface MAPL_PublishServer
      procedure :: mapl_publish_server
   end interface MAPL_PublishServer

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

      ! Always create the default input/output servers and clients so that
      ! simple utilities (e.g. Regrid_Util.x) that never call MAPL_CreateServers
      ! can use pfio I/O without extra setup.
      call this%initialize_default_servers(_RC)

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
         call normalize_mapl_hconfig(this%hconfig, this%mapl_hconfig, _RC)
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
         call normalize_mapl_hconfig(this%hconfig, this%mapl_hconfig, _RC)
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

      subroutine normalize_mapl_hconfig(hconfig, mapl_hconfig, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_HConfig), intent(inout) :: mapl_hconfig
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_model_petcount, has_app_petcount
         logical :: has_servers, has_pflogger, has_pflogger_cfg_file
         type(ESMF_HConfig) :: servers_hconfig
         character(:), allocatable :: pflogger_cfg_file

         has_model_petcount = ESMF_HConfigIsDefined(mapl_hconfig, keystring='model_petcount', _RC)
         has_app_petcount = ESMF_HConfigIsDefined(hconfig, keystring='app_petcount', _RC)
         if (.not. has_model_petcount .and. has_app_petcount) then
            call ESMF_HConfigAdd(mapl_hconfig, content=ESMF_HConfigAsI4(hconfig, keystring='app_petcount', _RC), &
                 addKeyString='model_petcount', _RC)
         end if

         has_servers = ESMF_HConfigIsDefined(mapl_hconfig, keystring='servers', _RC)
         if (.not. has_servers .and. ESMF_HConfigIsDefined(hconfig, keystring='servers', _RC)) then
            servers_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='servers', _RC)
            call ESMF_HConfigAdd(mapl_hconfig, content=servers_hconfig, addKeyString='servers', _RC)
            call ESMF_HConfigDestroy(servers_hconfig, _RC)
         end if

         has_pflogger_cfg_file = ESMF_HConfigIsDefined(mapl_hconfig, keystring='pflogger_cfg_file', _RC)
         has_pflogger = ESMF_HConfigIsDefined(hconfig, keystring='pflogger', _RC)
         if (.not. has_pflogger_cfg_file .and. has_pflogger) then
            pflogger_cfg_file = ESMF_HConfigAsString(hconfig, keystring='pflogger', _RC)
            call ESMF_HConfigAdd(mapl_hconfig, content=pflogger_cfg_file, addKeyString='pflogger_cfg_file', _RC)
         end if

         _RETURN(_SUCCESS)
      end subroutine normalize_mapl_hconfig

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
      character(ESMF_MAXSTR), allocatable :: server_names(:)
      integer, allocatable :: ssis_per_server(:)
      class(Logger), pointer :: lgr

      has_server_section = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='servers', _RC)
      _RETURN_UNLESS(has_server_section)

      call this%get_vm_topology(ssiMap=ssiMap, ssiCount=ssiCount, world_comm=world_comm, _RC)
      model_petCount = get_model_petcount(this%mapl_vm, this%mapl_hconfig, _RC)
      num_model_ssis = get_num_ssis(model_petCount, ssiMap, ssiOffset=0, _RC)

      servers_hconfig = ESMF_HConfigCreateAt(this%mapl_hconfig, keystring='servers', _RC)
      call get_server_hconfigs(servers_hconfig, server_hconfigs, server_names, _RC)
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

   ! Initialize the default input/output servers and clients unconditionally.
   ! Called from initialize() so that simple utilities (e.g. Regrid_Util.x) that
   ! never call MAPL_CreateServers can still perform pfio I/O.
   ! Uses the full world communicator as the model communicator (all PETs are
   ! model PETs).  MAPL_CreateServers will re-partition if a servers: section
   ! is present in the configuration.
   subroutine initialize_default_servers(this, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: world_comm

      call ESMF_VMGet(this%mapl_vm, mpiCommunicator=world_comm, _RC)

      ! Duplicate the world communicator so model_comm can be safely freed in finalize()
      ! or replaced by create_servers() if a servers: section partitions it.
      call MPI_Comm_dup(world_comm, this%model_comm, _IERROR)
      this%is_model_pet = .true.

      ! DirectoryService needs the full world communicator so all PETs can discover ports.
      this%directory_service = DirectoryService(world_comm)

      ! Create the two default local servers and their clients.
      ! Input uses a standard ClientThread; output uses a FastClientThread.
      call this%add_local_server(MAPL_DEFAULT_INPUT_SERVER, MAPL_DEFAULT_INPUT_SERVER, _RC)
      call this%add_local_server(MAPL_DEFAULT_OUTPUT_SERVER, MAPL_DEFAULT_OUTPUT_SERVER, fast_client=.true., _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_default_servers

   ! Create non-default server communicators and ESMF GridComps when an explicit
   ! servers: section is present.  The default input/output servers created in
   ! initialize() remain available.  This routine re-partitions model_comm to
   ! exclude server PETs, then adds any local: true servers from the config and
   ! builds ESMF GridComps for the remote servers.
   ! Returns server GridComps via servers(:); an empty array if no servers: section.
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

      has_server_section = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='servers', _RC)
      if (.not. has_server_section) then
         ! Simple path: default servers already created in initialize(); nothing to do.
         allocate(servers(0))
         _RETURN(_SUCCESS)
      end if

      ! Complex path: re-partition model_comm to the model-only PETs, add any
      ! local: true servers from config, and build remote server GridComps.
      call this%get_vm_topology(ssiMap=ssiMap, ssiCount=ssiCount, world_comm=world_comm, _RC)
      model_petCount = get_model_petcount(this%mapl_vm, this%mapl_hconfig, _RC)

      ! Free the world-spanning model_comm set in initialize_default_servers and
      ! replace it with the model-only partition.
      if (this%model_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%model_comm, _IERROR)
      end if
      call MPI_Comm_group(world_comm, world_group, _IERROR)
      call MPI_Group_range_incl(world_group, 1, reshape([0, model_petCount-1, 1], [3,1]), model_group, _IERROR)
      call MPI_Comm_create_group(world_comm, model_group, 0, this%model_comm, _IERROR)
      call MPI_Group_free(model_group, _IERROR)
      call MPI_Group_free(world_group, _IERROR)
      this%is_model_pet = (this%model_comm /= MPI_COMM_NULL)

      ! Add any local: true servers declared in the servers: section.
      if (this%is_model_pet) then
         call this%initialize_configured_local_servers(_RC)
      end if

      ! Build ESMF GridComps for remote (non-local) servers.
      call this%initialize_non_default_servers(servers, world_comm, model_petCount, ssiCount, ssiMap, &
           is_model_pet=this%is_model_pet, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine create_servers

   ! Build MPI communicators and ESMF GridComps for an explicit servers: topology.
   subroutine initialize_non_default_servers(this, servers, world_comm, model_petCount, ssiCount, ssiMap, &
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
       integer :: n_servers
       logical :: is_local, has_subclass, has_nwriter
       integer :: ssi_0, ssi_1, i_server, i_all
       integer, allocatable :: ssis_per_server(:)
       integer, allocatable :: model_pets(:), server_pets(:)
      type(ESMF_HConfig), allocatable :: server_hconfigs(:)
      character(ESMF_MAXSTR), allocatable :: server_names(:)
      class(Logger), pointer :: lgr
      type(ServerResources) :: srv_resources

      num_model_ssis = get_num_ssis(model_petCount, ssiMap, ssiOffset=0, _RC)

      servers_hconfig = ESMF_HConfigCreateAt(this%mapl_hconfig, keystring='servers', _RC)
      call get_server_hconfigs(servers_hconfig, server_hconfigs, server_names, _RC)

      ! get_ssis_per_server handles '*' wildcard for the last server.
      ! Resource validation already ran in MAPL_Initialize; this is belt-and-suspenders.
      ssis_per_server = get_ssis_per_server(server_hconfigs, ssiCount=ssiCount, num_model_ssis=num_model_ssis, _RC)

      call MPI_Comm_group(world_comm, world_group, _IERROR)
      model_pets = pets_on_ssis(ssiMap, 0, num_model_ssis)
      call MPI_Group_incl(world_group, size(model_pets), model_pets, model_group, _IERROR)
      ! model_comm already created in create_servers; derive is_model_pet from it.
      if (present(is_model_pet)) is_model_pet = (this%model_comm /= MPI_COMM_NULL)

      ssi_0 = num_model_ssis
      n_servers = count_remote_servers(server_hconfigs, _RC)
      allocate(servers(n_servers))

      ! Populate only remote (non-local) servers
      i_server = 0
      do i_all = 1, size(server_hconfigs)
         ! Skip local servers (already handled in initialize_configured_local_servers)
         is_local = is_local_server(server_hconfigs(i_all), _RC)
         if (is_local) then
            cycle
         end if
         
         i_server = i_server + 1
         ssi_1 = ssi_0 + ssis_per_server(i_all)
         server_pets = pets_on_ssis(ssiMap, ssi_0, ssi_1)
         call create_server_comms(world_comm, world_group, model_group, server_pets, server_comm, model_server_comm, _RC)

         srv_resources%world_comm = model_server_comm
         srv_resources%model_comm = this%model_comm
         srv_resources%server_comm = server_comm
         srv_resources%subclass = 'MpiServer'
         has_subclass = ESMF_HConfigIsDefined(server_hconfigs(i_all), keystring='subclass', _RC)
         if (has_subclass) srv_resources%subclass = &
              ESMF_HConfigAsString(server_hconfigs(i_all), keystring='subclass', _RC)
         srv_resources%nwriter_per_node = 1
         has_nwriter = ESMF_HConfigIsDefined(server_hconfigs(i_all), keystring='nwriter_per_node', _RC)
         if (has_nwriter) srv_resources%nwriter_per_node = &
              ESMF_HConfigAsI4(server_hconfigs(i_all), keystring='nwriter_per_node', _RC)

         servers(i_server) = make_server_gridcomp(trim(server_names(i_all)), server_hconfigs(i_all), &
              server_pets, srv_resources, _RC)
         ssi_0 = ssi_1
      end do

      call MPI_Group_Free(model_group, _IERROR)
      call MPI_Group_Free(world_group, _IERROR)
      call ESMF_HConfigDestroy(servers_hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_non_default_servers

   ! Initialize servers declared with local: true in the servers: config section.
   ! Called from create_servers() when a servers: section is present.
   ! The two default servers (MAPL_DEFAULT_INPUT_SERVER, MAPL_DEFAULT_OUTPUT_SERVER)
   ! are NOT created here — they were already created in initialize_default_servers().
   subroutine initialize_configured_local_servers(this, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

       integer :: status
       type(ESMF_HConfig) :: servers_hconfig
       type(ESMF_HConfig) :: server_val
       character(:), allocatable :: server_name
       logical :: is_local
       type(ESMF_HConfigIter) :: iter_begin, iter_end, iter

       ! Iterate the servers: section and register any local: true entries.
       servers_hconfig = ESMF_HConfigCreateAt(this%mapl_hconfig, keystring='servers', _RC)
       iter_begin = ESMF_HConfigIterBegin(servers_hconfig, _RC)
       iter_end   = ESMF_HConfigIterEnd(servers_hconfig, _RC)
       iter       = iter_begin

       do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
          server_name = ESMF_HConfigAsStringMapKey(iter, _RC)
          server_val = ESMF_HConfigCreateAtMapVal(iter, _RC)
          is_local = ESMF_HConfigIsDefined(server_val, keystring='local', _RC)
          if (is_local) is_local = ESMF_HConfigAsLogical(server_val, keystring='local', _RC)
          if (is_local) then
             call this%add_local_server(server_name, make_client_name(server_name), hconfig=server_val, _RC)
          end if
          call ESMF_HConfigDestroy(server_val, _RC)
       end do


      call ESMF_HConfigDestroy(servers_hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_configured_local_servers

   ! Register one local server and connect its client in a single atomic step.
   ! server_name: key used in local_server_map and DirectoryService port registry.
   ! client_name: key used in the pfio ClientManager.
   ! hconfig: optional — if provided, reads subclass: to dispatch server type.
   !          If not provided, defaults to MpiServer.
   ! fast_client: if .true., use FastClientThread for the client; default is ClientThread.
   subroutine add_local_server(this, server_name, client_name, hconfig, fast_client, rc)
      class(MaplFramework), target, intent(inout) :: this
      character(*), intent(in) :: server_name
      character(*), intent(in) :: client_name
       type(ESMF_HConfig), optional, intent(in) :: hconfig
      logical, optional, intent(in) :: fast_client
      integer, optional, intent(out) :: rc

      integer :: status, alloc_stat
      class(BaseServer), allocatable :: tmp
      class(BaseServer), pointer :: srv
      class(ClientThread), allocatable :: new_client
      class(ClientThread), pointer :: p_client
      logical :: has_subclass, use_fast
      character(:), allocatable :: subclass_name

      ! Determine server subclass.
      subclass_name = 'MpiServer'  ! default
      if (present(hconfig)) then
         has_subclass = ESMF_HConfigIsDefined(hconfig, keystring='subclass', _RC)
         if (has_subclass) subclass_name = ESMF_HConfigAsString(hconfig, keystring='subclass', _RC)
      end if

      ! Allocate appropriate server subclass.
      select case (trim(subclass_name))
      case ('MpiServer')
         allocate(tmp, source=MpiServer(this%model_comm, server_name, rc=status), stat=alloc_stat)
         _VERIFY(status)
         _VERIFY(alloc_stat)
      case ('MultiGroupServer')
         allocate(tmp, source=MultiGroupServer(this%model_comm, server_name, nwriter_per_node=1, rc=status), &
              stat=alloc_stat)
         _VERIFY(status)
         _VERIFY(alloc_stat)
      case default
         _ASSERT(.false., "Unknown server subclass: '"//trim(subclass_name)//"'")
      end select

      ! Publish the server so connect_to_server can find it by name.
      call this%local_server_map%insert(server_name, tmp)
      srv => this%local_server_map%at(server_name)
      call this%directory_service%publish(PortInfo(server_name, srv), srv)

      ! Create the client, store it in the ClientManager, then connect the
      ! map-resident copy to the server.  connect_to_server stores a pointer to
      ! the client internally (SimpleSocket), so the client must be stored in
      ! stable long-lived storage (the ClientManager map) before connecting.
      use_fast = .false.
      if (present(fast_client)) use_fast = fast_client

      if (use_fast) then
         allocate(new_client, source=FastClientThread(client_comm=this%model_comm, rc=status))
      else
         allocate(new_client, source=ClientThread(client_comm=this%model_comm, rc=status))
      end if
      _VERIFY(status)

      call add_client(client_name, new_client, _RC)
      p_client => get_client(client_name, _RC)
      call this%directory_service%connect_to_server(server_name, p_client)

      _RETURN(_SUCCESS)
   end subroutine add_local_server

    ! Run servers on server PETs; model PETs return immediately.
    ! Server GridComps register only run phase.
    subroutine run_servers(this, servers, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: servers(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i, status

      ! Model PETs have nothing to do here.
      _RETURN_IF(this%is_model_pet)

       ! Server PETs run each server GridComp.
       ! ESMF only executes on PETs in the GridComp's petList; other
       ! server PETs silently skip GridComps they don't belong to.
       do i = 1, size(servers)
          call ESMF_GridCompRun(servers(i), _RC)
       end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_servers

   subroutine mapl_connect_to_server(server_name, unusable, client_name, rc)
      character(*), intent(in) :: server_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: client_name
      integer, optional, intent(out) :: rc

      character(:), allocatable :: client_name_
      class(ClientThread), allocatable :: client
      class(ClientThread), pointer :: p_client
      integer :: status

      client_name_ = server_name
      if (present(client_name)) client_name_ = client_name

      allocate(client, source=FastClientThread(client_comm=the_mapl_object%model_comm, rc=status))
      _VERIFY(status)
      call add_client(client_name_, client, _RC)
      p_client => get_client(client_name_, _RC)
      call the_mapl_object%directory_service%connect_to_server(server_name, p_client)

      n_ext_clients = n_ext_clients + 1
      ext_client_names(n_ext_clients) = client_name_

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_connect_to_server

   subroutine mapl_publish_server(server_name, server, rc)
      character(*), intent(in) :: server_name
      class(BaseServer), target, intent(inout) :: server
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%directory_service%publish(PortInfo(server_name, server), server, _RC)
      call the_mapl_object%directory_service%connect_to_client(server_name, server, _RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_publish_server

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

      call this%finalize_servers(_RC)

      call this%directory_service%free_directory_resources()
      if (this%model_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%model_comm, _IERROR)
      end if
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

      integer :: i, status
      class(ClientThread), pointer :: p_client

      ! Model PETs send terminate to each external server client so server
      ! PETs can exit server%start() and reach ESMF_Finalize collectively.
      if (this%is_model_pet) then
         do i = 1, n_ext_clients
            p_client => get_client(trim(ext_client_names(i)), _RC)
            call p_client%terminate(_RC)
         end do
      end if

      ! local_server_map owns o_server and i_server (and any future local servers).
      ! MpiServer uses allocatable components so clearing the map triggers
      ! automatic cleanup of server resources.
      call this%local_server_map%clear()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
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

    ! Helper function to derive client name from server name.
    ! Convention: 'foo_server' -> 'foo_client', 'foo' -> 'foo_client'.
    function make_client_name(server_name) result(client_name)
       character(*), intent(in) :: server_name
       character(:), allocatable :: client_name
       integer :: pos

       ! Look for '_server' suffix
       pos = index(server_name, '_server', back=.true.)
       if (pos > 0) then
          ! Replace _server with _client
          allocate(character(len=pos-1+7) :: client_name)
          client_name = server_name(1:pos-1) // '_client'
       else
          ! Append _client
          allocate(character(len=len_trim(server_name)+7) :: client_name)
          client_name = trim(server_name) // '_client'
       end if
    end function make_client_name

    ! Helper function to check if a server hconfig has local: true.
    function is_local_server(server_hconfig, rc) result(is_local)
       type(ESMF_HConfig), intent(in) :: server_hconfig
       integer, optional, intent(out) :: rc
       logical :: is_local

       integer :: status

       is_local = ESMF_HConfigIsDefined(server_hconfig, keystring='local', _RC)
       if (is_local) then
          is_local = ESMF_HConfigAsLogical(server_hconfig, keystring='local', _RC)
       end if

       _RETURN(_SUCCESS)
    end function is_local_server

    ! Helper function to count the number of remote (non-local) server entries.
    function count_remote_servers(server_hconfigs, rc) result(count)
       type(ESMF_HConfig), intent(in) :: server_hconfigs(:)
       integer, optional, intent(out) :: rc
       integer :: count

       integer :: status
       logical :: is_local
       integer :: i_server

       count = 0
       do i_server = 1, size(server_hconfigs)
          is_local = is_local_server(server_hconfigs(i_server), _RC) 
          if (.not. is_local) then
             count = count + 1
          end if
       end do

       _RETURN(_SUCCESS)
    end function count_remote_servers

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

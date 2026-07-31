#include "MAPL.h"

module mapl_NuopcMetaModel_mod
   use esmf
   use nuopc
   use mapl_MethodPhasesMap_mod
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use mapl_ErrorHandling_mod
   use mapl_InnerMetaComponent_mod
   use mapl_GriddedComponentDriver_mod
   use mapl_GriddedComponentDriverVector_mod
   use mapl_GriddedComponentDriverMap_mod
   use mapl_ComponentSpec_mod
   use mapl_UserSetServices_mod
   use mapl_VariableSpec_mod
   use mapl_VariableSpecVector_mod
   use mapl_MultiState_mod
   use mapl_enums_api
!   use gFTLv2_StringVector

   implicit none(type,external)
   private

   public :: NuopcMetaModel
   public :: attach_meta_model
   public :: get_meta_model

   type NuopcMetaModel
      private
      type(ESMF_GridComp) :: self_model
      type(GriddedComponentDriver) :: user_gc_driver
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(ESMF_HConfig) :: hconfig
      type(ComponentSpec)                         :: component_spec
      type(MethodPhasesMap)                       :: user_phases_map
      logical :: run_if_alarm_rings_next = .FALSE.
   contains
      procedure :: init
      procedure :: setServices => setServices_
      procedure :: get_phases
      procedure :: run_custom
      procedure :: get_user_gc_driver
      ! Init phases
      !------------
      ! label_Advertise
      procedure :: advertise
      procedure :: advertise_geom_a
      procedure :: advertise_geom_b
      procedure :: advertise_variable
      !label_ModifyAdvertise
      procedure :: modify_advertise
      !label_RealizeAccept
      procedure :: realize_accept
      !label_RealizeProvided
      procedure :: realize_provided
      ! label_DataInitialize
      procedure :: data_initialize
      procedure :: read_restart
      procedure :: user_initialize

      ! Run phases
      !------------
      ! label_Advance
      procedure :: advance
      ! label_WriteRestart
      procedure :: write_restart
      ! label_AdvanceClock
      procedure :: advance_clock

      ! Finalize phases
      !----------------
      ! label_Finalize
      procedure :: finalize
   end type NuopcMetaModel

   interface NuopcMetaModel
      module procedure :: construct_meta_model
   end interface NuopcMetaModel

   interface get_meta_model
      module procedure :: get_meta_model_from_generic_model
   end interface get_meta_model

   character(len=*), parameter :: META_MODEL_PRIVATE_STATE = "MAPL::NuopcMetaModel::private"

contains

   function construct_meta_model(model, user_gc_driver, user_setservices, hconfig) result(meta_model)
      type(NuopcMetaModel) :: meta_model
      type(ESMF_GridComp), intent(in) :: model
      type(GriddedComponentDriver), intent(in) :: user_gc_driver
      class(AbstractUserSetServices), intent(in) :: user_setservices
      type(ESMF_HConfig), intent(in) :: hconfig
      
      meta_model%self_model = model
      meta_model%user_gc_driver = user_gc_driver
      allocate(meta_model%user_setServices, source=user_setServices)
      meta_model%hconfig = hconfig
      call initialize_phases_map(meta_model%user_phases_map)

   end function construct_meta_model

   !wdb fixme deleteme To be implemented
   subroutine init(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: user_gc_name

      user_gc_name = this%user_gc_driver%get_name(_RC)
      !wdb fixme deleteme Do we set the registry?
      !wdb fixme deleteme Do we set the logger?
      _RETURN(_SUCCESS)

   end subroutine init

   !wdb fixme deleteme To be implemented
   subroutine setServices_(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: user_name
      type(ESMF_GridComp) :: user_gridcomp

      user_name = this%user_gc_driver%get_name()
      user_gridcomp = this%user_gc_driver%get_gridcomp()
      call attach_inner_meta(user_gridcomp, this%self_model, _RC)
      call this%user_setservices%run(user_gridcomp, _RC)
      !wdb fixme deleteme Need to parse hconfig to get component_spec, but no registry. Need component_spec for other procedures
      ! this%component_spec = parse_component_spec(this%hconfig, this%registry, user_name, _RC)

      _RETURN(_SUCCESS)

   end subroutine setServices_

   subroutine advertise(this, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      ! Extra MAPL phases
      call this%advertise_geom_a()
      call this%advertise_geom_b()
      
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ADVERTISE, _RC)
      call self_advertise(this, _RC)
 
      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine self_advertise(this, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      associate (e => this%component_spec%var_specs%end())
        iter = this%component_spec%var_specs%begin()
        do while (iter /= e)
           var_spec => iter%of()
           call this%advertise_variable(var_spec, _RC)
           call iter%next()
        end do
      end associate
 
      _RETURN(_SUCCESS)
   end subroutine self_advertise

   subroutine advertise_variable(this, var_spec, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      type(VariableSpec), intent(in) :: var_spec
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_State) :: state

!      select case (var_spec%state_intent)
!      case (ESMF_STATEINTENT_IMPORT)
!         call nuopc_ModelGet(this%self_model, importState=state, _RC)
!      case (ESMF_STATEINTENT_EXPORT)
!         call nuopc_ModelGet(this%self_model, exportState=state, _RC)
!      case default
!         _FAIL('Unsupported state intent')
!      end select

!      call nuopc_advertise(state, &
!           standardName=var_spec%standard_name, &
!           name=var_spec%short_name, _RC)

   end subroutine advertise_variable
   
   !wdb fixme deleteme Not implemented
   subroutine modify_advertise(this, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

   end subroutine modify_advertise

   recursive subroutine read_restart(this, unusable, rc)
      class(NuopcMetaModel), target, intent(inout) :: this
      ! optional arguments
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_READ_RESTART'
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
!      type(RestartHandler) :: restart_handler
      character(:), allocatable :: subdir
      character(:), allocatable :: filename
      type(ESMF_Time) :: current_time
      integer :: status
!      class(Logger), pointer :: user_logger
      logical :: bootstrap

!      call recurse(this, phase_idx=MAPL_GENERIC_INIT_READ_RESTART, _RC)
!      call this%run_custom(ESMF_METHOD_READRESTART, PHASE_NAME, _RC)

!      _RETURN_UNLESS(this%has_geom())

      driver => this%get_user_gc_driver()
      states = driver%get_states()
      call ESMF_ClockGet(driver%get_clock(), currTime=current_time, _RC)

!      user_logger => this%get_logger()
! wdb deleteme fixme Need to restore or replace this eventually
      !restart_handler = RestartHandler(this%get_geom(), current_time, user_logger)

      ! if I try to pass this derived type in to read in folowing code nag crashes
      !wdb fixme deleteme Disable temporily.
!      bootstrap = this%component_spec%misc%restart_controls%get_bootstrap()
!      if (this%component_spec%misc%restart_controls%get_import()) then
!         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_IMPORT, _RC)
!         call this%start_timer("ReadImportRestart", _RC)
!         call restart_handler%read(states%importState, filename, bootstrap, _RC)
!         call this%stop_timer("ReadImportRestart", _RC)
!      end if

!      if (this%component_spec%misc%restart_controls%get_internal()) then
!         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_INTERNAL, _RC)
!         call this%start_timer("ReadInternalRestart", _RC)
!         call restart_handler%read(states%internalState, filename, bootstrap, _RC)
!         call this%stop_timer("ReadInternalRestart", _RC)
!      end if
      !wdb fixme deleteme END Disable temporily.

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine read_restart

   subroutine data_initialize(this, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      ! Extra MAPL phases
      call this%read_restart(_RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ADVERTISE, _RC)
!      call this%init_user_gc(_RC)
 
      _RETURN(_SUCCESS)
   end subroutine data_initialize

   subroutine advance(this, phaseLabel, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      character(ESMF_MAXSTR), intent(in) :: phaseLabel
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Clock) :: clock
!      type(StringVector), pointer :: run_phases
      logical :: found
!      class(logger_t), pointer :: logger
      integer :: currentPhase, phase

      type(ESMF_Time) :: currTime
      logical :: is_ringing

!      call ESMF_GridCompGet(this%self_model, currentPhase=currentPhase, _RC)
!      call nuopc_GridCompSearchRevPhaseMap(model, ESMF_METHOD_RUN, currentPhase, phaseLabel=phaseLabel, _RC)
      
!      phase = get_phase_index(run_phases, phase_name, found=found)

      call ESMF_GridCompGet(this%self_model, clock=clock, _RC)
      call ESMF_ClockGet(clock, currTime=currTime, _RC)
!      if (this%run_if_alarm_rings_next) then
!         call ESMF_ClockGetNextTime(clock, nextTime=currTime, _RC)
!      end if
!      is_ringing = this%user_run_alarm%is_ringing(currTime, _RC)
!      _RETURN_IF(.not. is_ringing)

!      run_phases => this%get_phases(ESMF_METHOD_RUN)
!      phase = get_phase_index(run_phases, trim(phaseLabel), found=found)
!      _ASSERT(found, 'phase <'//trim(phaseLabel)//'> not found for model <'//this%get_name()//'>')

!      logger => this%get_logger()
      !wdb fixme deleteme Need to reactivate
      !call logger%info(phase_name//": starting...")
      call this%start_timer(phase_name)
!      call this%user_gc_driver%run(phase_idx=phase, _RC)
      call this%stop_timer(phase_name)
      !wdb fixme deleteme Need to reactivate
      !call logger%info(phase_name//": ...completed")

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine advance
   

   subroutine advance_clock(this, modelClock, unusable, rc)
      class(NuopcMetaModel), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: modelClock
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
!      type(StringVector), pointer :: run_phases
      logical :: found
      logical :: is_ringing
      integer :: phase
      type(ESMF_Time) :: currTime

!      call nuopc_ModelGet(this%self_model, modelClock=modelCloc, RC)
      call ESMF_ClockGet(modelClock, currTime=currTime, _RC)
!      if (this%run_if_alarm_rings_next) then
!         call ESMF_ClockGetNextTime(clock, nextTime=currTime, _RC)
!      end if
      !wdb fixme deleteme Need to reactivate
      !is_ringing = this%user_run_alarm%is_ringing(currTime, _RC)
      is_ringing = .FALSE.
      _RETURN_IF(.not. is_ringing)

      !wdb fixme deleteme Need to reactivate
      !associate(e => this%children%ftn_end())
        !iter = this%children%ftn_begin()
        !do while (iter /= e)
           !call iter%next()
           !child => iter%second()
            !call child%run(phase_idx=MAPL_GENERIC_RUN_CLOCK_ADVANCE, _RC)
           !call child%clock_advance()
        !end do
      !end associate

      !call this%user_gc_driver%clock_advance(_RC)

      ! Check for customization
      !run_phases => this%get_phases(ESMF_METHOD_RUN)
      !phase = get_phase_index(run_phases, phase_name='GENERIC::RUN_CLOCK_ADVANCE', found=found)
      !if (found) then
      !   call this%user_gc_driver%run(phase_idx=phase, _RC)
      !end if
      !wdb fixme deleteme END Need to reactivate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine advance_clock

   function get_phases(this, method_flag) result(phases)
!      type(StringVector), pointer :: phases
      integer :: phases !wdb fixme deleteme This is a temporary workaround.
      class(NuopcMetaModel), target, intent(inout):: this
      type(ESMF_Method_Flag), intent(in) :: method_flag

!      phases => this%user_phases_map%of(method_flag)

   end function get_phases

   subroutine attach_meta_model(model, rc)
      type(ESMF_GridComp), intent(inout) :: model
      integer, optional, intent(out) :: rc
      integer :: status
      
      _SET_NAMED_PRIVATE_STATE(model, NuopcMetaModel, META_MODEL_PRIVATE_STATE)
      _RETURN(_SUCCESS)

   end subroutine attach_meta_model

   function get_meta_model_from_generic_model(model, rc) result(ptr)
      class(NuopcMetaModel), pointer :: ptr
      type(ESMF_GridComp), intent(inout) :: model
      integer, optional, intent(out) :: rc
      integer :: status
      
      _GET_NAMED_PRIVATE_STATE(model, NuopcMetaModel, META_MODEL_PRIVATE_STATE, ptr)
      _RETURN(_SUCCESS)

   end function get_meta_model_from_generic_model

   !wdb fixme deleteme Not implemented
   subroutine advertise_geom_a(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine advertise_geom_a

   !wdb fixme deleteme Not implemented
   subroutine advertise_geom_b(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine advertise_geom_b

   !wdb fixme deleteme Not implemented
   subroutine recurse(meta_model, phase_idx, rc)
      class(NuopcMetaModel), target, intent(inout) :: meta_model
      integer, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine recurse

   !wdb fixme deleteme Not implemented
   subroutine realize_accept(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine realize_accept

   !wdb fixme deleteme Not implemented
   subroutine realize_provided(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine realize_provided

   !wdb fixme deleteme Not implemented
   subroutine user_initialize(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine user_initialize

   !wdb fixme deleteme Not implemented
   subroutine write_restart(this, importState, exportState, clock, unusable, rc)
      class(NuopcMetaModel), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      
      _RETURN(_SUCCESS)

   end subroutine write_restart

   !wdb fixme deleteme Not implemented
   subroutine finalize(this, importState, exportState, clock, unusable, rc)
      class(NuopcMetaModel), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: state
      
      _RETURN(_SUCCESS)

   end subroutine finalize

   !wdb fixme deleteme Not implemented
   subroutine run_custom(this, methodFlag, phaseName, rc)
      class(NuopcMetaModel), intent(inout) :: this
      type(ESMF_Method_Flag), intent(in) :: methodFlag
      character(len=*), intent(in) :: phaseName
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine run_custom

   function get_user_gc_driver(this) result(ptr)
      type(GriddedComponentDriver), pointer :: ptr
      class(NuopcMetaModel), target, intent(in) :: this
      ptr => this%user_gc_driver
   end function get_user_gc_driver

   !wdb fixme deleteme Not implemented
   subroutine init_user_gc(this, rc)
      class(NuopcMetaModel), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine init_user_gc

   !wdb fixme deleteme Not implemented
   subroutine start_timer(this, name, rc)
      class(NuopcMetaModel), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine start_timer

   !wdb fixme deleteme Not implemented
   subroutine stop_timer(this, name, rc)
      class(NuopcMetaModel), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine stop_timer

end module mapl_NuopcMetaModel_mod

#include "MAPL.h"

module mapl_NuopcMetaModel_mod
   use mapl_KeywordEnforcer_mod
   use esmf
   use nuopc
   implicit none(type,external)
   private

   public :: NuopcMetaModel
   public :: make_model


   type NupcMetaModel
      private
      type(esmf_GridComp) :: self_model
      type(GriddedComponentDriver) :: user_gc_driver
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(ComponentSpec)                         :: component_spec

   contains
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

   end type NupcMetaModel

contains

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
      type(esmf_State) :: state

      select case (var_spec%state_intent)
      case (ESMF_STATEINTENT_IMPORT)
         call nuopc_ModelGet(this%self_model, importState=state, _RC)
      case (ESMF_STATEINTENT_EXPORT)
         call nuopc_ModelGet(this%self_model, exportState=state, _RC)
      case default
         _FAIL('Unsupported state intent')
      end select

      call nuopc_advertise(state, &
           standardName=var_spec%standard_name, &
           name=var_spec%short_name, _RC)

   end subroutine advertise_variable
   
   subroutine modify_advertise(this, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

   end subroutine modify_advertise


   recursive subroutine read_restart(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_READ_RESTART'
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      type(RestartHandler) :: restart_handler
      character(:), allocatable :: subdir
      character(:), allocatable :: filename
      type(esmf_Time) :: current_time
      integer :: status
      class(Logger), pointer :: user_logger
      logical :: bootstrap

      call recurse(this, phase_idx=MAPL_GENERIC_INIT_READ_RESTART, _RC)
      call this%run_custom(ESMF_METHOD_READRESTART, PHASE_NAME, _RC)

      _RETURN_UNLESS(this%has_geom())

      driver => this%get_user_gc_driver()
      states = driver%get_states()
      call esmf_ClockGet(driver%get_clock(), currTime=current_time, _RC)

      user_logger => this%get_logger()
      restart_handler = RestartHandler(this%get_geom(), current_time, user_logger)

      ! if I try to pass this derived type in to read in folowing code nag crashes
      bootstrap = this%component_spec%misc%restart_controls%get_bootstrap()
      if (this%component_spec%misc%restart_controls%get_import()) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_IMPORT, _RC)
         call this%start_timer("ReadImportRestart", _RC)
         call restart_handler%read(states%importState, filename, bootstrap, _RC)
         call this%stop_timer("ReadImportRestart", _RC)
      end if

      if (this%component_spec%misc%restart_controls%get_internal()) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_INTERNAL, _RC)
         call this%start_timer("ReadInternalRestart", _RC)
         call restart_handler%read(states%internalState, filename, bootstrap, _RC)
         call this%stop_timer("ReadInternalRestart", _RC)
      end if

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

      call this%init_user_gc(_RC)
 
      _RETURN(_SUCCESS)
   end subroutine data_initialize


   subroutine advance((this, unusable, rc)
      class(NuopcMetaModel), intent(inout) ::  this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Clock) :: modelClock
      type(StringVector), pointer :: run_phases
      logical :: found
      class(logger_t), pointer :: logger
      integer :: currentPhase, phase

      type(ESMF_Time) :: currTime
      logical :: is_ringing
      integere :: currentPhase
      character(ESMF_MAXSTR) :: phaseLabel

      call esmf_GridCompGet(model, currentPhase=currentPhase, _RC)
      call nuopc_GridCompSearchRevPhaseMap(model, ESMF_METHOD_RUN, currentPhase, phaseLabel=phaseLabel, _RC)
      
      phase = get_phase_index(run_phases, phase_name, found=found)

      call nuopc_ModelGet(this%self_model, modelClock=modelClock, _RC)
      call ESMF_ClockGet(modelClock, currTime=currTime, _RC)
      if (this%run_if_alarm_rings_next) then
         call ESMF_ClockGetNextTime(clock, nextTime=currTime, _RC)
      end if
      is_ringing = this%user_run_alarm%is_ringing(currTime, _RC)
      _RETURN_IF(.not. is_ringing)

      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, trim(phaseLabel), found=found)
      _ASSERT(found, 'phase <'//trim(phaseLabel)//'> not found for model <'//this%get_name()//'>')

      logger => this%get_logger()
      call logger%info(phase_name//": starting...")
      call this%start_timer(phase_name)
      call this%user_gc_driver%run(phase_idx=phase, _RC)
      call this%stop_timer(phase_name)
      call logger%info(phase_name//": ...completed")

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine advance
   

   subroutine advance_clock(this, unusable, rc)
      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(StringVector), pointer :: run_phases
      logical :: found
      logical :: is_ringing
      integer :: phase
      type(ESMF_Time) :: currTime

      call nuopc_ModelGet(this%self_model, modelClock=modelCloc, RC)
      call ESMF_ClockGet(modelClock, currTime=currTime, _RC)
      if (this%run_if_alarm_rings_next) then
         call ESMF_ClockGetNextTime(clock, nextTime=currTime, _RC)
      end if
      is_ringing = this%user_run_alarm%is_ringing(currTime, _RC)
      _RETURN_IF(.not. is_ringing)

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           child => iter%second()
            call child%run(phase_idx=MAPL_GENERIC_RUN_CLOCK_ADVANCE, _RC)
           call child%clock_advance()
        end do
      end associate

      call this%user_gc_driver%clock_advance(_RC)

      ! Check for customization
      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, phase_name='GENERIC::RUN_CLOCK_ADVANCE', found=found)
      if (found) then
         call this%user_gc_driver%run(phase_idx=phase, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine advance_clock

end module mapl_NuopcMetaModel

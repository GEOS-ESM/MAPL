#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) SetServices_smod
   use mapl3g_ComponentSpecParser
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_GenericGridComp
   use mapl3g_BasicVerticalGrid
   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling
   implicit none

contains

   ! Note we spell the following routine with trailing underscore as a workaround
   ! for a bug in gfortran-12 that "leaks" private names into client code.
   !========================================================================
   ! Generic SetServices order of operations:
   !
   ! 1) Parse any generic aspects of the hconfig.
   ! 2) Add children from config
   ! 3) Create inner (user) gridcomp and call its setservices.
   !
   ! Note that specs are processed depth first, but that this may
   ! reverse when step (3) is moved to a new generic initialization phase.
   !=========================================================================
   
   recursive module subroutine SetServices_(this, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: user_gridcomp
      type(ESMF_Clock) :: user_clock, outer_clock
      type(ESMF_Time) :: reference_time
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: user_reference_time
      type(ESMF_TimeInterval) :: user_timestep

      call ESMF_GridCompGet(this%self_gridcomp, clock=outer_clock, _RC)
      call ESMF_ClockGet(outer_clock, refTime=reference_time, timeStep=timeStep, _RC)
      
      this%component_spec = parse_component_spec(this%hconfig, this%registry, &
           timeStep=timeStep, reference_time=reference_time, _RC)
      user_gridcomp = this%user_gc_driver%get_gridcomp()
      user_clock = this%user_gc_driver%get_clock()
      call ESMF_ClockGet(user_clock, refTime=user_reference_time, timeStep=user_timestep, _RC)

      call check_reference_times_are_compatible(reference_time, user_reference_time, timestep, user_timestep, _RC)
      call ESMF_ClockSet(user_clock, timeStep=timestep, _RC)
      call ESMF_ClockSet(user_clock, refTime=reference_time, _RC)

      call set_run_user_alarm(this, outer_clock, user_clock, _RC)

      call attach_inner_meta(user_gridcomp, this%self_gridcomp, _RC)
      call this%user_setservices%run(user_gridcomp, _RC)
      call add_children(this, _RC)
      call run_children_setservices(this, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      recursive subroutine add_children(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(ChildSpecMapIterator) :: iter
         type(ChildSpec), pointer :: child_spec
         type(ESMF_HConfig), allocatable :: child_hconfig
         character(:), allocatable :: child_name

         associate ( e => this%component_spec%children%ftn_end() )
           iter = this%component_spec%children%ftn_begin()
           do while (iter /= e)
              call iter%next()
              child_name = iter%first()
              child_spec => iter%second()

              if (allocated(child_spec%config_file)) then
                 child_hconfig = ESMF_HConfigCreate(filename=child_spec%config_file, rc=status)
                 _ASSERT(status==0,'problem with config file: '//child_spec%config_file)
              end if
              call this%add_child(child_name, child_spec%user_setservices, child_hconfig, _RC)
           end do
         end associate

         _RETURN(_SUCCESS)
      end subroutine add_children

      ! By now children have either been added by specs or by direct
      ! calls in the parent gc's setservices.
      recursive subroutine run_children_setservices(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status, user_status
         type(GriddedComponentDriver), pointer :: child_comp
         type(ESMF_GridComp) :: child_outer_gc
         type(GriddedComponentDriverMapIterator) :: iter


         associate ( e => this%children%ftn_end() )
            iter = this%children%ftn_begin()
            do while (iter /= e)
               call iter%next()
               child_comp => iter%second()
               child_outer_gc = child_comp%get_gridcomp()
               call ESMF_GridCompSetServices(child_outer_gc, generic_setservices, _USERRC)
            end do
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine run_children_setservices

   end subroutine SetServices_

   subroutine set_run_user_alarm(this, outer_clock, user_clock,  rc)
      use mapl_ErrorHandling
      class(OuterMetaComponent), intent(in) :: this
      type(ESMF_Clock), intent(inout) :: outer_clock
      type(ESMF_Clock), intent(inout) :: user_clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TimeInterval) :: outer_timestep, user_timestep, zero
      type(ESMF_Time) :: refTime
      type(ESMF_Alarm) :: alarm

      call ESMF_TimeIntervalSet(zero, s=0, _RC)

      call ESMF_ClockGet(outer_clock, timestep=outer_timestep, _RC)
      call ESMF_ClockGet(user_clock, timestep=user_timestep, refTime=refTime, _RC)

      _ASSERT(mod(user_timestep, outer_timestep) == zero, 'User timestep is not an integer multiple of parent timestep')

      alarm = ESMF_AlarmCreate(outer_clock, &
           name = RUN_USER_ALARM, &
           ringInterval=user_timestep, &
           refTime=refTime, &
           sticky=.false., &
           _RC)

      call ESMF_AlarmRingerOn(alarm, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_run_user_alarm

   subroutine check_timesteps_are_compatible(timestep1, timestep2, rc)
      type(ESMF_TimeInterval), intent(in) :: timestep1
      type(ESMF_TimeInterval), intent(in) :: timestep2
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: ERRMSG = 'Timesteps are not compatible.'
      
      call get_duration_type(timestep1, timestep2, _RC)
      _ASSERT(mod(timestep2, timestep1) == ZERO, ERRMSG)
      _ASSERT(mod(timestep1, timestep2) == ZERO, ERRMSG)
      _RETURN(_SUCCESS)

   end subroutine check_timesteps_are_compatible

   subroutine check_reference_times_are_compatible(reftime1, reftime2, timestep1, timestep2, rc)
      type(ESMF_Time), intent(in) :: reftime1
      type(ESMF_Time), intent(in) :: reftime2
      type(ESMF_TimeInterval), intent(in) :: timestep1
      type(ESMF_TimeInterval), intent(in) :: timestep2
      integer, optional, intent(in) :: rc
      integer :: status
      logical :: compatible
      type(ESMF_TimeInterval) :: difference

      call check_timesteps_are_compatible(timestep1, timestep2, _RC)
      compatible = mod(reftime1 - reftime2, timestep1)
      if(reftime1 < reftime2) compatible = mod(reftime2 - reftime1, timestep2)
      _ASSERT(compatible, 'Reference times are not compatible.')
      _RETURN(_SUCCESS)

   end subroutine check_reference_times_are_compatible

   logical function timestep_is_monthly(timestep, rc)
      type(ESMF_TimeInterval), intent(in) :: timestep
      integer, optional, intent(out) :: rc 
      integer :: status
      integer :: mm
      logical :: yearly

      yearly = timestep_is_yearly(timestep, _RC)
      call ESMF_TimeIntervalGet(timestep, mm=mm, _RC)
      timestep_is_monthly = .not. yearly .and. mm /= 0
      _RETURN(_SUCCESS)

   end function timestep_is_monthly(timestep, rc)

   logical function timestep_is_yearly(timestep, rc)
      type(ESMF_TimeInterval), intent(in) :: timestep
      integer, optional, intent(out) :: rc 
      integer :: status
      integer :: yy

      call ESMF_TimeIntervalGet(timestep, yy=yy, _RC)
      timestep_is_yearly = yy /= 0
      _RETURN(_SUCCESS)

   end function timestep_is_yearly(timestep, rc)

   subroutine get_duration_type(timestep, timestep2, rc)
      type(ESMF_TimeInterval), intent(in) :: timestep
      type(ESMF_TimeInterval), intent(in) :: timestep2
      integer, optional, intent(out) :: rc 
      integer :: status
      logical :: lval
      logical :: lval2
      character(len=*), parameter :: ERRMSG = 'Timesteps are different duration types'

      lval = timestep_is_monthly(timestep, _RC)
      lval2 = timestep_is_monthly(timestep2, _RC)
      _ASSERT(lval .eqv. lval2, ERRMSG)
      _RETURN_IF(lval)

      lval = timestep_is_yearly(timestep, _RC)
      lval2 = timestep_is_yearly(timestep2, _RC)
      _ASSERT(lval .eqv. lval2, ERRMSG)
      
   end subroutine get_duration_type

end submodule SetServices_smod

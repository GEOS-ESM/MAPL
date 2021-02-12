#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_MaplGenericComponent
   use ESMF
   use mapl_AbstractFrameworkComponent
   use mapl_BaseFrameworkComponent
   use mapl_keywordenforcermod
   use mapl_AbstractComponent
   use mapl_CompositeComponent
   use mapl_ConcreteComposite
   use mapl_MaplComponent
   use mapl_ErrorHandlingMod
   use pFlogger
   implicit none
   private

   public :: MaplGenericComponent

   type, extends(BaseFrameworkComponent) :: MaplGenericComponent
!!$      private
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
   contains
      procedure :: initialize => stub
      procedure :: run => stub
      procedure :: finalize => stub

      procedure :: initialize_child => stub_child
      procedure :: run_child => stub_child
      procedure :: finalize_child => stub_child

      procedure :: add_child_component


      ! accessors

      procedure :: get_logger
      procedure :: set_logger
   end type MaplGenericComponent

contains



   subroutine stub(this, clock, phase, unusable, rc)
      class(MaplGenericComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractComponent), pointer :: user_component

      _UNUSED_DUMMY(unusable)

      user_component => this%get_component()
      call user_component%run(this%import_state, this%export_state, clock, phase, __RC__)

      _RETURN(_SUCCESS)
   end subroutine stub
   
   subroutine stub_child(this, name, clock, phase, unusable, rc)
      class(MaplGenericComponent), intent(inout) :: this
      character(*), intent(in) :: name
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class(AbstractFrameworkComponent), pointer :: child
      integer :: status

      _UNUSED_DUMMY(unusable)

      child => this%get_child(name)
      call child%run(clock, phase, __RC__)

      _RETURN(_SUCCESS)
   end subroutine stub_child


   function add_child_component(this, name, user_component) result(child)
      class(AbstractFrameworkComponent), pointer :: child
      class(MaplGenericComponent), target, intent(inout) :: this
      character(*), intent(in) :: name
      class(AbstractComponent), intent(in) :: user_component

      type(MaplGenericComponent) :: tmp

      child => this%add_child(name, tmp)
      call child%set_component(user_component)

   end function add_child_component


   subroutine generic_entry_point(gridcomp, import, export, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: import
      type(ESMF_State), intent(inout) :: export
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR) :: name
      integer :: phase
      type(ESMF_Method_Flag) :: method
      integer :: status

      class(MaplComponent), pointer :: component
      type :: ComponentWrapper
         class(MaplComponent), pointer :: component
      end type ComponentWrapper

      type(ComponentWrapper) :: wrapper
      character(:), pointer :: phase_name
      
      call ESMF_UserCompGetInternalState(gridcomp, "MaplComponent", wrapper, status)
      _VERIFY(status)
      component => wrapper%component

      call ESMF_GridCompGet( gridcomp, name=name, currentPhase=phase, currentMethod=method, __RC__)

      if (method == ESMF_METHOD_INITIALIZE) then

!!$         phase_name => component%init_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%initialize(import, export, clock, phase_name, __RC__)

      else if (method == ESMF_METHOD_RUN) then

!!$         phase_name = component%run_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%run(import, export, clock, phase_name, __RC__)

      else if (method == ESMF_METHOD_FINALIZE) then
         
!!$         phase_name = component%finalize_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%finalize(import, export, clock, phase_name, __RC__)

      else
         _FAIL('unknown value for ESMF_METHOD_FLAG')
      end if
         
      _RETURN(_SUCCESS)
   end subroutine generic_entry_point

   function get_logger(this) result(lgr)
      class(Logger), pointer :: lgr
      class(MaplGenericComponent), intent(in) :: this
      class(AbstractComponent), pointer :: component

      component => this%get_component()
      lgr => component%get_logger()
      
   end function get_logger

   subroutine set_logger(this, lgr)
      class(MaplGenericComponent), intent(inout) :: this
      class(Logger), target :: lgr
      class(AbstractComponent), pointer :: component

      component => this%get_component()
      call component%set_logger(lgr)
      
   end subroutine set_logger

end module mapl_MaplGenericComponent

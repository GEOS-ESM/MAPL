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
   use mapl_MaplGrid
   implicit none
   private

   public :: MaplGenericComponent
   public :: get_grid

   abstract interface
      subroutine UserMethod(gc, import, export, clock, rc)
         use ESMF
         implicit none
         type (ESMF_GridComp), intent(inout) :: gc     ! Gridded component
         type (ESMF_State),    intent(inout) :: import ! Import state
         type (ESMF_State),    intent(inout) :: export ! Export state
         type (ESMF_Clock),    intent(inout) :: clock  ! The clock
         integer, optional,    intent(  out) :: rc     ! Error code:
      end subroutine UserMethod
   end interface

   type, extends(BaseFrameworkComponent) :: MaplGenericComponent
!!$      private
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_State) :: internal_state

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
      procedure :: get_internal_state
      procedure :: get_import_state
      procedure :: get_export_state
      procedure :: get_grid
      procedure :: get_gridcomp
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
      call user_component%run(this%import_state, this%export_state, clock, phase, _RC)

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
      call child%run(clock, phase, _RC)

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

   function get_internal_state(this) result(internal_state)
      class(MaplGenericComponent), target, intent(in) :: this
      type(ESMF_State), pointer :: internal_state

      internal_state => this%internal_state
   end function get_internal_state

   function get_import_state(this) result(import_state)
     class(MaplGenericComponent), target, intent(in) :: this
     type(ESMF_State), pointer :: import_state

     import_state => this%import_state
   end function get_import_state

   function get_export_state(this) result(export_state)
     class(MaplGenericComponent), target, intent(in) :: this
     type(ESMF_State), pointer :: export_state

     export_state => this%export_state
   end function get_export_state

   function get_grid(this) result(grid)
     class(MaplGenericComponent), target, intent(in) :: this
     type(MaplGrid), pointer :: grid

     grid => this%grid
   end function get_grid


   function get_gridcomp(this) result(gridcomp)
      class(MaplGenericComponent), target, intent(in) :: this
      type(ESMF_GridComp), pointer :: gridcomp

      gridcomp => this%gridcomp
   end function get_gridcomp

end module mapl_MaplGenericComponent

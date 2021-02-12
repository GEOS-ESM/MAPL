#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_MaplComponent
   use ESMF
   use pFlogger, only: t_Logger => Logger
   use mapl_KeywordEnforcerMod
   use mapl_BaseComponent
   use mapl_SurrogateFrameworkComponent
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: MaplComponent

   type, abstract, extends(BaseComponent) :: MaplComponent
      private
      class(SurrogateFrameworkComponent), pointer :: framework => null()
      type(ESMF_State) :: internal_state
      class(t_Logger), pointer :: logger => null()
   contains
      ! Accessors
      procedure :: set_framework
      procedure :: get_framework
      procedure :: set_logger
      procedure :: get_logger

      procedure :: run_child

      procedure :: get_internal_state
   end type MaplComponent

contains

   subroutine set_framework(this, framework)
      class(MaplComponent), intent(inout) :: this
      class(Surrogateframeworkcomponent), target :: framework

      this%framework => framework
   end subroutine set_framework

   function get_framework(this) result(framework)
      class(Surrogateframeworkcomponent), pointer :: framework
      class(MaplComponent), intent(in) :: this

      framework => this%framework
   end function get_framework


   subroutine run_child(this, name, clock, phase, unusable, rc)
      class(MaplComponent), intent(inout) :: this
      character(*), intent(in) :: name
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)
      call this%framework%run_child(name, clock, phase, rc=status)

      _RETURN(_SUCCESS)
   end subroutine run_child

   function get_internal_state(this) result(internal_state)
      class(MaplComponent), target, intent(in) :: this
      type(ESMF_State), pointer :: internal_state

      internal_state => this%internal_state
   end function get_internal_state


   subroutine set_logger(this, logger)
      class(MaplComponent), intent(inout) :: this
      class(t_Logger), target :: logger

      this%logger => logger
   end subroutine set_logger

   function get_logger(this) result(logger)
      class(t_Logger), pointer :: logger
      class(MaplComponent), intent(in) :: this

      logger => this%logger
   end function get_logger


end module mapl_MaplComponent

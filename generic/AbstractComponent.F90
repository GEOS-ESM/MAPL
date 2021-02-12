module mapl_AbstractComponent
   implicit none(external)
   private

   public :: AbstractComponent

   type, abstract :: AbstractComponent
   contains
      ! Primary methods
      procedure(i_Run), deferred :: initialize
      procedure(i_Run), deferred :: run
      procedure(i_Run), deferred :: finalize

      ! Framework methods
      procedure(i_RunChild), deferred :: run_child

      ! Accessors
      procedure(i_SetName), deferred :: set_name
      procedure(i_GetName), deferred :: get_name
      procedure(i_SetFramework), deferred :: set_framework
      procedure(i_GetFramework), deferred :: get_framework
      procedure(i_SetLogger), deferred :: set_logger
      procedure(i_GetLogger), deferred :: get_logger

      procedure(i_GetState), deferred :: get_internal_state

   end type AbstractComponent


   abstract interface

      subroutine i_Run(this, import_state, export_state, clock, phase, unusable, rc)
         use mapl_KeywordEnforcerMod
         use ESMF
         import AbstractComponent
         implicit none
         class(AbstractComponent), intent(inout) :: this
         type(ESMF_State), intent(inout) :: import_state
         type(ESMF_State), intent(inout) :: export_state
         type(ESMF_Clock), intent(inout) :: clock
         character(*), intent(in) :: phase
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine i_Run

      subroutine i_SetName(this, name)
         import AbstractComponent
         implicit none
         class(AbstractComponent), intent(inout) :: this
         character(*), intent(in) :: name
      end subroutine i_SetName

      function i_GetName(this) result(name)
         import AbstractComponent
         implicit none
         character(:), allocatable :: name
         class(AbstractComponent), intent(in) :: this
      end function i_GetName


      subroutine i_SetFramework(this, framework)
         use mapl_SurrogateFrameworkComponent
         import AbstractComponent
         implicit none
         class(AbstractComponent), intent(inout) :: this
         class(SurrogateFrameworkComponent), target :: framework
      end subroutine i_SetFramework

      function i_GetFramework(this) result(framework)
         use mapl_SurrogateFrameworkComponent
         import AbstractComponent
         implicit none
         class(SurrogateFrameworkComponent), pointer :: framework
         class(AbstractComponent), intent(in) :: this
      end function i_GetFramework

      function i_GetState(this) result(state)
         use ESMF
         import AbstractComponent
         implicit none
         type(ESMF_State), pointer :: state
         class(AbstractComponent), target, intent(in) :: this
      end function i_GetState

      subroutine i_RunChild(this, name, clock, phase, unusable, rc)
         use mapl_KeywordEnforcerMod
         use ESMF
         import AbstractComponent
         implicit none
         class(AbstractComponent), intent(inout) :: this
         character(*), intent(in) :: name
         type(ESMF_Clock), intent(inout) :: clock
         character(*), intent(in) :: phase
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine i_RunChild

      subroutine i_SetLogger(this, logger)
         use pFlogger, only: t_Logger => Logger
         import AbstractComponent
         implicit none
         class(AbstractComponent), intent(inout) :: this
         class(t_Logger), target :: logger

      end subroutine i_SetLogger

      function i_GetLogger(this) result(logger)
         use pFlogger, only: t_Logger => Logger
         import AbstractComponent
         implicit none
         class(t_Logger), pointer :: logger
         class(AbstractComponent), intent(in) :: this
      end function i_GetLogger

   end interface
      
end module mapl_AbstractComponent

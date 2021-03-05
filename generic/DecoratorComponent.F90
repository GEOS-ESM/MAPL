#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_DecoratorComponent
   use ESMF
   use mapl_KeywordEnforcerMod
   use mapl_AbstractComponent
   use mapl_MaplComponent
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: DecoratorComponent

   type, abstract, extends(AbstractComponent) :: DecoratorComponent
      private
      class(MaplComponent), allocatable :: decorated
   contains
      procedure :: get_decorated
      procedure :: set_decorated

      procedure :: initialize
      procedure :: run
      procedure :: finalize

      procedure :: run_child

      procedure :: set_name
      procedure :: get_name
      procedure :: set_framework
   end type DecoratorComponent


contains

   subroutine set_decorated(this, decorated)
      class(DecoratorComponent), intent(inout) :: this
      class(MaplComponent), intent(in) :: decorated

      this%decorated = decorated

   end subroutine set_decorated

   function get_decorated(this) result(decorated)
      class(MaplComponent), pointer :: decorated
      class(DecoratorComponent), target, intent(in) :: this

      decorated => this%decorated

   end function get_decorated


   subroutine initialize(this, import_state, export_state, clock, phase, unusable, rc)
      class(DecoratorComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      type(ESMF_State), intent(inout) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      
      _UNUSED_DUMMY(unusable)
      call this%decorated%initialize(import_state, export_state, clock, phase, __RC__)
      
      _RETURN(_SUCCESS)
   end subroutine initialize

   
   subroutine run(this, import_state, export_state, clock, phase, unusable, rc)
      class(DecoratorComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      type(ESMF_State), intent(inout) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)
      call this%decorated%run(import_state, export_state, clock, phase, __RC__)
      
      _RETURN(_SUCCESS)
   end subroutine run

   
   subroutine finalize(this, import_state, export_state, clock, phase, unusable, rc)
      class(DecoratorComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      type(ESMF_State), intent(inout) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)
      call this%decorated%finalize(import_state, export_state, clock, phase, __RC__)

      _RETURN(_SUCCESS)
   end subroutine finalize

   subroutine run_child(this, name, clock, phase, unusable, rc)
      class(DecoratorComponent), intent(inout) :: this
      character(*), intent(in) :: name
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)
      call this%decorated%run_child(name, clock, phase, __RC__)

      _RETURN(_SUCCESS)
   end subroutine run_child

   subroutine set_name(this, name)
      class(DecoratorComponent), intent(inout) :: this
      character(*), intent(in) :: name

      call this%decorated%set_name(name)
   end subroutine set_name

   function get_name(this) result(name)
      character(:), allocatable :: name
      class(DecoratorComponent), intent(in) :: this

      name = this%decorated%get_name()
   end function get_name


   subroutine set_framework(this, framework)
      use mapl_SurrogateFrameworkComponent
      class(DecoratorComponent), intent(inout) :: this
      class(SurrogateFrameworkComponent), target :: framework

      call this%decorated%set_framework(framework)
      
   end subroutine set_framework

   function get_framework(this) result(framework)
      use mapl_SurrogateFrameworkComponent
      class(Surrogateframeworkcomponent), pointer :: framework
      class(DecoratorComponent), intent(in) :: this

      framework => this%get_framework()
   end function get_framework

   
end module mapl_DecoratorComponent

#include "MAPL_Generic.h"

module mapl3g_StateItemExtension
   use mapl3g_StateItemSpec
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_ExtensionAction
   use mapl3g_GenericCoupler
   use mapl3g_MultiState
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: StateItemExtension
   public :: StateItemExtensionPtr

   ! A StateItemExtension "owns" a spec as well as the coupler
   ! that produces it (if any).

   type StateItemExtension
      private
      class(StateItemSpec), allocatable :: spec
      type(GriddedComponentDriver), allocatable :: producer ! coupler that computes spec
      type(ComponentDriverPtrVector) :: consumers ! couplers that depend on spec
   contains
      procedure :: get_spec
      procedure :: get_producer
      procedure :: get_consumers
      procedure :: has_producer
      procedure :: add_consumer
      procedure :: make_extension
   end type StateItemExtension

   type :: StateItemExtensionPtr
      type(StateItemExtension), pointer :: ptr => null()
   end type StateItemExtensionPtr

   interface StateItemExtension
      procedure :: new_StateItemExtension_spec
      procedure :: new_StateItemExtension_w_producer
   end interface StateItemExtension

contains

   function new_StateItemExtension_spec(spec) result(ext)
      type(StateItemExtension) :: ext
      class(StateItemSpec), intent(in) :: spec
      ext%spec = spec
   end function new_StateItemExtension_spec

   function new_StateItemExtension_w_producer(spec, producer) result(ext)
      type(StateItemExtension) :: ext
      class(StateItemSpec), intent(in) :: spec
      type(GriddedComponentDriver), intent(in) :: producer
      ext%spec = spec
      ext%producer = producer
   end function new_StateItemExtension_w_producer

   function get_spec(this) result(spec)
      class(StateItemExtension), target, intent(in) :: this
      class(StateItemSpec), pointer :: spec
      spec => this%spec
   end function get_spec

   logical function has_producer(this)
      class(StateItemExtension), target, intent(in) :: this
      has_producer = allocated(this%producer)
   end function has_producer

   function get_producer(this) result(producer)
      class(StateItemExtension), target, intent(in) :: this
      type(GriddedComponentDriver), pointer :: producer
      if (.not. allocated(this%producer)) then
         producer => null()
      end if
      
      producer => this%producer

   end function get_producer

   function get_consumers(this) result(consumers)
      class(StateItemExtension), target, intent(in) :: this
      type(ComponentDriverPtrVector), pointer :: consumers
      consumers => this%consumers
   end function get_consumers

   subroutine add_consumer(this, consumer)
      class(StateItemExtension), intent(inout) :: this
      type(GriddedComponentDriver), pointer :: consumer
      type(ComponentDriverPtr) :: wrapper

      wrapper%ptr => consumer
      call this%consumers%push_back(wrapper)
   end subroutine add_consumer

   ! Creation of an extension requires a new coupler that transforms
   ! from source (this) spec to dest (extension) spec. This new coupler
   ! is added to the export specs of source (this), and the new extension
   ! gains it as a reference (pointer).

   function make_extension(this, goal, rc) result(extension)
      type(StateItemExtension), target :: extension
      class(StateItemExtension), target, intent(inout) :: this
      class(StateItemSpec), target, intent(in) :: goal
      integer, intent(out) :: rc

      integer :: status
      class(StateItemSpec), allocatable :: new_spec
      class(ExtensionAction), allocatable :: action
      type(GriddedComponentDriver) :: producer
      type(ESMF_GridComp) :: coupler_gridcomp
      type(ESMF_Clock) :: fake_clock

      call this%spec%new_make_extension(goal, new_spec, action, _RC)
!#      new_spec = this%spec%make_extension(goal, _RC)
      call new_spec%create(_RC)
      call new_spec%set_active()
      call this%spec%set_active

!#      action = this%spec%make_action(new_spec, _RC)
      coupler_gridcomp = make_coupler(action, _RC)
      producer = GriddedComponentDriver(coupler_gridcomp, fake_clock, MultiState())

      extension = StateItemExtension(new_spec, producer)

      _RETURN(_SUCCESS)
   end function make_extension

end module mapl3g_StateItemExtension

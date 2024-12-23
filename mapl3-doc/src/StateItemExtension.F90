#include "MAPL_Generic.h"

module mapl3g_StateItemExtension
   use mapl3g_StateItemSpec
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_ExtensionAction
   use mapl3g_GenericCoupler
   use mapl3g_StateItemAspect
   use mapl3g_MultiState
   use mapl_ErrorHandling
   use gftl2_StringVector
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
      procedure :: set_producer
      procedure :: get_consumers
      procedure :: has_producer
      procedure :: has_consumers
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

   logical function has_consumers(this)
      class(StateItemExtension), target, intent(in) :: this
      has_consumers = this%consumers%size() > 0
   end function has_consumers

   function get_producer(this) result(producer)
      class(StateItemExtension), target, intent(in) :: this
      type(GriddedComponentDriver), pointer :: producer

      producer => null()
      if (.not. allocated(this%producer)) return
      producer => this%producer

   end function get_producer

   subroutine set_producer(this, producer, rc)
      class(StateItemExtension), intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: producer
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%has_producer(), 'cannot set producer for extension that already has one')
      this%producer = producer

      _RETURN(_SUCCESS)
   end subroutine set_producer

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

   recursive function make_extension(this, goal, rc) result(extension)
      type(StateItemExtension), target :: extension
      class(StateItemExtension), target, intent(inout) :: this
      class(StateItemSpec), target, intent(in) :: goal
      integer, intent(out) :: rc

      integer :: status
      integer :: i
      class(StateItemSpec), allocatable :: new_spec
      class(ExtensionAction), allocatable :: action
      type(GriddedComponentDriver) :: producer
      type(GriddedComponentDriver), pointer :: source
      type(ESMF_GridComp) :: coupler_gridcomp
      type(StateItemAdapterWrapper), allocatable :: adapters(:)
      type(ESMF_Clock) :: fake_clock
      logical :: match
      type(StringVector), target :: aspect_names
      character(:), pointer :: aspect_name
      class(StateItemAspect), pointer :: src_aspect, dst_aspect


      call this%spec%set_active()

      new_spec = this%spec

      aspect_names = this%spec%get_aspect_order(goal)
      do i = 1, aspect_names%size()
         aspect_name => aspect_names%of(i)
         src_aspect => new_spec%get_aspect(aspect_name, _RC)
         dst_aspect => goal%get_aspect(aspect_name, _RC)
         _ASSERT(src_aspect%can_connect_to(dst_aspect), 'cannoct connect aspect ' // aspect_name)

         if (src_aspect%needs_extension_for(dst_aspect)) then
            allocate(action, source=src_aspect%make_action(dst_aspect, rc=status))
            _VERIFY(status)
            call new_spec%set_aspect(dst_aspect, _RC)
            exit
         end if

      end do

      if (allocated(action)) then
         call new_spec%create(_RC)
         call new_spec%set_active()
         source => this%get_producer()
         coupler_gridcomp = make_coupler(action, source, _RC)
         producer = GriddedComponentDriver(coupler_gridcomp, fake_clock, MultiState())
         extension = StateItemExtension(new_spec, producer)
         _RETURN(_SUCCESS)
      end if

      ! The logic below should be removed once Aspects have fully
      ! replaced Adapters.
      adapters = this%spec%make_adapters(goal, _RC)
      do i = 1, size(adapters)
         match = adapters(i)%adapter%match(new_spec, _RC)
         if (match) cycle
         call adapters(i)%adapter%adapt(new_spec, action, _RC)
         exit
      end do

      if (.not. allocated(action)) then
         extension = StateItemExtension(this%spec)
         _RETURN(_SUCCESS)
      end if

      call new_spec%create(_RC)
      call new_spec%set_active()

      source => this%get_producer()
      coupler_gridcomp = make_coupler(action, source, _RC)
      producer = GriddedComponentDriver(coupler_gridcomp, fake_clock, MultiState())
      extension = StateItemExtension(new_spec, producer)

      _RETURN(_SUCCESS)
   end function make_extension

end module mapl3g_StateItemExtension

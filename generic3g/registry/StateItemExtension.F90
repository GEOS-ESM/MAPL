#include "MAPL.h"

module mapl3g_StateItemExtension
   use mapl3g_GenericCoupler
   use mapl3g_StateItemSpec
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriverVector
   use mapl3g_ExtensionTransform
   use mapl3g_GenericCoupler
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl_ErrorHandling
   use esmf
   implicit none(type, external)
   private

   public :: StateItemExtension
   public :: StateItemExtensionPtr

   ! A StateItemExtension "owns" a spec as well as the coupler
   ! that produces it (if any).

   type StateItemExtension
      private
      type(StateItemSpec) :: spec
      type(ComponentDriverVector) :: consumers ! couplers that depend on spec
      class(ComponentDriver), pointer :: producer => null() ! coupler that computes spec
   contains
      procedure :: get_spec

      procedure :: has_producer
      procedure :: get_producer
      procedure :: set_producer

      procedure :: has_consumers
      procedure :: add_consumer
      procedure :: get_consumers

      procedure :: make_extension
      procedure :: is_deferred
   end type StateItemExtension

   type :: StateItemExtensionPtr
      type(StateItemExtension), pointer :: ptr => null()
   end type StateItemExtensionPtr

   interface StateItemExtension
      procedure :: new_StateItemExtension_spec
   end interface StateItemExtension

contains

   function new_StateItemExtension_spec(spec) result(ext)
      type(StateItemExtension) :: ext
      type(StateItemSpec), intent(in) :: spec
      ext%spec = spec
   end function new_StateItemExtension_spec

   function get_spec(this) result(spec)
      class(StateItemExtension), target, intent(in) :: this
      type(StateItemSpec), pointer :: spec
      spec => this%spec
   end function get_spec

   logical function has_producer(this)
      class(StateItemExtension), target, intent(in) :: this
      has_producer = associated(this%producer)
   end function has_producer

   function get_producer(this) result(producer)
      class(StateItemExtension), target, intent(in) :: this
      class(ComponentDriver), pointer :: producer

      producer => this%producer

   end function get_producer

   subroutine set_producer(this, producer, rc)
      class(StateItemExtension), intent(inout) :: this
      class(ComponentDriver), pointer, intent(in) :: producer
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%has_producer(), 'cannot set producer for extension that already has one')
      this%producer => producer

      _RETURN(_SUCCESS)
   end subroutine set_producer


  logical function has_consumers(this)
      class(StateItemExtension), target, intent(in) :: this
      has_consumers = this%consumers%size() > 0
   end function has_consumers


   function get_consumers(this) result(consumers)
      class(StateItemExtension), target, intent(in) :: this
      type(ComponentDriverVector), pointer :: consumers
      consumers => this%consumers
   end function get_consumers

function add_consumer(this, consumer, rc) result(reference)
      class(ComponentDriver), pointer :: reference
      class(StateItemExtension), target, intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: consumer
      integer, optional, intent(out) :: rc

      integer :: status

      call this%consumers%push_back(consumer)
      reference => this%consumers%back()
      _RETURN_UNLESS(associated(this%producer))
      
      call mapl_CouplerAddConsumer(this%producer, reference, _RC)

      _RETURN(_SUCCESS)
   end function add_consumer

   ! Creation of an extension requires a new coupler that transforms
   ! from source (this) spec to dest (extension) spec.
   ! This coupler is a "consumer" of the original spec (this), and a "producer" of
   ! the new spec (extension).

   recursive function make_extension(this, goal, rc) result(extension)
      type(StateItemExtension) :: extension
      class(StateItemExtension), target, intent(inout) :: this
      type(StateItemSpec), target, intent(in) :: goal
      integer, intent(out) :: rc

      integer :: status
      integer :: i
      type(StateItemSpec), target :: new_spec
      class(ExtensionTransform), allocatable :: transform
      class(ComponentDriver), pointer :: producer
      class(ComponentDriver), pointer :: source
      type(ESMF_GridComp) :: coupler_gridcomp
      logical :: match
      type(AspectId), allocatable :: aspect_ids(:)
      class(StateItemAspect), pointer :: src_aspect, dst_aspect
      type(AspectMap), pointer :: other_aspects

      call this%spec%activate(_RC)
      call this%spec%update_from_payload(_RC)

      new_spec = this%spec

      aspect_ids = this%spec%get_aspect_order(goal)
      do i = 1, size(aspect_ids)

         src_aspect => new_spec%get_aspect(aspect_ids(i), _RC)
         _ASSERT(associated(src_aspect), 'src aspect not found')

         dst_aspect => goal%get_aspect(aspect_ids(i), _RC)
         _ASSERT(associated(dst_aspect), 'dst aspect not found')

         _ASSERT(src_aspect%can_connect_to(dst_aspect), 'cannot connect aspect ' // aspect_ids(i)%to_string())
         if (src_aspect%needs_extension_for(dst_aspect)) then
            other_aspects => new_spec%get_aspects()
            allocate(transform, source=src_aspect%make_transform(dst_aspect, other_aspects, rc=status))
            _VERIFY(status)
            call new_spec%set_aspect(dst_aspect, _RC)

            exit
         end if

      end do

      if (allocated(transform)) then
         call new_spec%create(_RC)
         call new_spec%activate(_RC)
         source => this%get_producer()
         coupler_gridcomp = make_coupler(transform, source, _RC)
         producer => this%add_consumer(GriddedComponentDriver(coupler_gridcomp), _RC)
         extension = StateItemExtension(new_spec)
         call extension%set_producer(producer)

         _RETURN(_SUCCESS)
      end if

      _RETURN(_SUCCESS)
   end function make_extension

   logical function is_deferred(this)
      class(StateItemExtension), target, intent(in) :: this

      is_deferred = this%spec%has_deferred_aspects()
   end function is_deferred

end module mapl3g_StateItemExtension

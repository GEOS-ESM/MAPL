#include "MAPL.h"

! Extensions procedures for StateRegistry:
! - add_virtual_pt: Add a virtual connection point
! - add_family: Add an extension family
! - add_primary_spec: Add primary spec for a virtual point
! - get_primary_extension: Get primary extension
! - add_extension: Add an extension to the registry
! - add_spec: Add a spec as an extension
! - link_extension: Link an extension to a virtual point
! - get_extension_family: Get the family for a virtual point
! - get_extensions: Get all extensions for a virtual point
! - extend: Recursively extend family to match goal spec
! - item_is_deferred: Check if item is deferred

submodule (mapl3g_StateRegistry) StateRegistry_Extensions_smod
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt, only: ActualConnectionPt
   implicit none(type,external)

contains

   module subroutine add_virtual_pt(this, virtual_pt, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%has_virtual_pt(virtual_pt), "Virtual connection point already exists in registry")
      call this%family_map%insert(virtual_pt, ExtensionFamily())

      _RETURN(_SUCCESS)
   end subroutine add_virtual_pt

   module subroutine add_family(this, virtual_pt, family, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(ExtensionFamily), intent(in) :: family
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: new_family
      
      call this%add_virtual_pt(virtual_pt, _RC)
      new_family => this%family_map%at(virtual_pt, _RC)
#ifndef __GFORTRAN__      
      new_family = family
#else
      call ridiculous(new_family, family)
#endif

      _RETURN(_SUCCESS)

#ifdef __GFORTRAN__      
   contains

      subroutine ridiculous(a, b)
         type(ExtensionFamily), intent(out) :: a
         type(ExtensionFamily), intent(in) :: b
         a = b
      end subroutine ridiculous
#endif

   end subroutine add_family

   module subroutine add_primary_spec(this, virtual_pt, spec, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension) :: extension
      type(ExtensionFamily) :: family

      extension = StateItemExtension(spec)
      call this%owned_items%push_back(extension)
      family = ExtensionFamily(this%owned_items%back())
      call this%add_family(virtual_pt, family, _RC)
      
      _RETURN(_SUCCESS)

   end subroutine add_primary_spec

   module function get_primary_extension(this, virtual_pt, rc) result(primary)
      type(StateItemExtension), pointer :: primary
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family

      primary => null()
      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")
      family => this%family_map%at(virtual_pt,_RC)
      primary => family%get_primary()


      _RETURN(_SUCCESS)
   end function get_primary_extension

   module function add_extension(this, virtual_pt, extension, rc) result(new_extension)
      type(StateItemExtension), pointer :: new_extension
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemExtension), intent(in) :: extension
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")

      call this%owned_items%push_back(extension)
      new_extension => this%owned_items%back()
      call this%link_extension(virtual_pt, this%owned_items%back(), _RC)

      _RETURN(_SUCCESS)
   end function add_extension

   module subroutine add_spec(this, virtual_pt, spec, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension) :: extension

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")

      extension = StateItemExtension(spec)
      call this%owned_items%push_back(extension)
      call this%link_extension(virtual_pt, this%owned_items%back(), _RC)

      _RETURN(_SUCCESS)
   end subroutine add_spec

   module subroutine link_extension(this, virtual_pt, extension, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemExtension), pointer, intent(in) :: extension
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")

      family => this%family_map%at(virtual_pt, _RC)
      call family%add_extension(extension)

      _RETURN(_SUCCESS)
   end subroutine link_extension

   module function get_extension_family(this, virtual_pt, rc) result(family)
      type(ExtensionFamily), pointer :: family
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      family => this%family_map%at(virtual_pt, _RC)

      _RETURN(_SUCCESS)
   end function get_extension_family

   module function get_extensions(this, virtual_pt, rc) result(extensions)
      type(StateItemExtensionPtr), allocatable :: extensions(:)
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family
      integer :: i, n

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")
      family => this%family_map%at(virtual_pt, _RC)
      n = family%num_variants()
      allocate(extensions(n))
      do i = 1, n
         extensions(i)%ptr => family%get_extension(i)
      end do

      _RETURN(_SUCCESS)
   end function get_extensions

   ! Repeatedly extend family at v_pt until extension can directly
   ! connect to goal_spec.
   recursive module function extend(registry, v_pt, goal_spec, rc) result(extension)
      type(StateItemExtension), pointer :: extension
      class(StateRegistry), target, intent(inout) :: registry
      type(VirtualConnectionPt), intent(in) :: v_pt
      type(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(StateItemExtension), pointer :: closest_extension, new_extension
      type(StateItemExtension) :: tmp_extension
      type(ExtensionFamily), pointer :: family
      class(ComponentDriver), pointer :: producer
      integer :: iter_count
      integer, parameter :: MAX_ITERATIONS = 10
      integer :: status
      type(MultiState) :: coupler_states
      type(ActualConnectionPt) :: a_pt
      type(StateItemSpec), pointer :: last_spec, new_spec

      family => registry%get_extension_family(v_pt, _RC)

      closest_extension => family%find_closest_extension(goal_spec, _RC)
      iter_count = 0
      do
         iter_count = iter_count + 1
          _ASSERT(iter_count <= MAX_ITERATIONS, "StateItem extensions for v_pt did not converge.")

          ! Leave commented code here.   This should be migrated to use pflogger in the future.
          ! Useful debugging point.
          
!#          block
!#            type(StateItemSpec), pointer :: spec
!#            spec => closest_extension%get_spec()
!#            _HERE, 'extending? ', iter_count
!#            call spec%print_spec(__FILE__,__LINE__)
!#          end block
         tmp_extension = closest_extension%make_extension(goal_spec, _RC)
         if (.not. associated(tmp_extension%get_producer())) exit ! no further extensions needed

         ! Add permanent copy of extension to registry and retrieve a valid pointer:
         new_extension => registry%add_extension(v_pt, tmp_extension, _RC)
         producer => new_extension%get_producer()

         coupler_states = producer%get_states()
         a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='import', short_name='import[1]'))
         last_spec => closest_extension%get_spec()
         call last_spec%activate(_RC)
         call last_spec%add_to_state(coupler_states, a_pt, _RC)
         a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='export', short_name='export[1]'))
         new_spec => new_extension%get_spec()
         call new_spec%add_to_state(coupler_states, a_pt, _RC)

         closest_extension => new_extension
      end do

      extension => closest_extension

      _RETURN(_SUCCESS)
   end function extend

   module function item_is_deferred(this, v_pt, rc) result(is_deferred)
      logical :: is_deferred
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: v_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family

      is_deferred = .false.
      _RETURN_UNLESS(v_pt%is_export())

      family => this%get_extension_family(v_pt, _RC)
      is_deferred = family%is_deferred(_RC)

      _RETURN(_SUCCESS)
   end function item_is_deferred

end submodule StateRegistry_Extensions_smod

#include "MAPL.h"

! A StateItem can be extended by means of a coupler.  The
! set of all such related extensions are encapsulated
! in objects of type ExtensionFamily.


module mapl3g_ExtensionFamily
   use mapl3g_StateItemSpec
   use mapl3g_StateItemSpecPtrVector
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl_ErrorHandling
   use gFTL2_StringVector
   implicit none(type,external)
   private

   public :: ExtensionFamily

   ! The primary/base item spec is tracked separately to enable
   ! control of which will appear in user states with its short-name.
   type :: ExtensionFamily
      private
      logical :: has_primary_ = .false.
      type(StateItemSpecPtrVector) :: specs
   contains
      procedure :: has_primary
      procedure :: get_primary
      procedure :: get_specs
      procedure :: get_spec
      procedure :: add_extension
      procedure :: num_variants
      procedure :: merge
      procedure :: is_deferred

      procedure :: find_closest_spec
      procedure :: get_primary_spec
   end type ExtensionFamily

   interface ExtensionFamily
      procedure new_ExtensionFamily_empty
      procedure new_ExtensionFamily_primary
   end interface ExtensionFamily

contains

   function new_ExtensionFamily_empty() result(family)
      type(ExtensionFamily) :: family
      family%has_primary_ = .false.
   end function new_ExtensionFamily_empty

   function new_ExtensionFamily_primary(primary) result(family)
      type(ExtensionFamily) :: family
      type(StateItemSpec), pointer, intent(in) :: primary

      type(StateItemSpecPtr) :: wrapper

      family%has_primary_ = .true.
      wrapper%ptr => primary
      call family%specs%push_back(wrapper)

   end function new_ExtensionFamily_primary

   logical function has_primary(this)
      class(ExtensionFamily), intent(in) :: this
      has_primary = this%has_primary_
   end function has_primary

   function get_primary(this, rc) result(primary)
      type(StateItemSpec), pointer :: primary
      class(ExtensionFamily), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      type(StateItemSpecPtr), pointer :: wrapper

      primary => null()
      _ASSERT(this%has_primary_, "No primary item spec")
      _ASSERT(this%specs%size() > 0, "No primary item spec")
      wrapper => this%specs%front()
      primary => wrapper%ptr
      _RETURN(_SUCCESS)
   end function get_primary

   function get_specs(this) result(extensions)
      type(StateItemSpecPtrVector), pointer :: extensions
      class(ExtensionFamily), target, intent(in) :: this
      extensions => this%specs
   end function get_specs

   function get_spec(this, i) result(extension)
      type(StateItemSpec), pointer :: extension
      integer, intent(in) :: i
      class(ExtensionFamily), target, intent(in) :: this

      type(StateItemSpecPtr), pointer :: wrapper
      wrapper => this%specs%at(i)
      extension => wrapper%ptr
   end function get_spec

   subroutine add_extension(this, extension)
      class(ExtensionFamily), intent(inout) :: this
      class(StateItemSpec), pointer, intent(in) :: extension

      type(StateItemSpecPtr) :: wrapper

      wrapper%ptr => extension
      call this%specs%push_back(wrapper)

   end subroutine add_extension

   integer function num_variants(this)
      class(ExtensionFamily), intent(in) :: this
      num_variants = this%specs%size()
   end function num_variants


   function find_closest_spec(family, goal_spec, rc) result(closest_extension)
      type(StateItemSpec), pointer :: closest_extension
      class(ExtensionFamily), intent(in) :: family
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtrVector) :: subgroup, new_subgroup
      class(StateItemSpec), pointer :: archetype
      integer :: i, j
      integer :: status
      type(StateItemSpecPtr) :: extension_ptr
      type(StateItemSpec), pointer :: primary
      class(StateItemSpec), pointer :: spec
      logical :: match
      type(AspectId), allocatable :: aspect_ids(:)

      class(StateItemAspect), pointer :: src_aspect, dst_aspect

      closest_extension => null()
      subgroup = family%get_specs()
      primary => family%get_primary()  ! archetype defines the rules
      archetype => primary
      ! new
      aspect_ids = archetype%get_aspect_order(goal_spec)
      do i = 1, size(aspect_ids)
         dst_aspect => goal_spec%get_aspect(aspect_ids(i), _RC)
         _ASSERT(associated(dst_aspect), 'expected aspect '// aspect_ids(i)%to_string() //' is missing')

         ! Find subset that match current aspect
         new_subgroup = StateItemSpecPtrVector()
         do j = 1, subgroup%size()
            extension_ptr = subgroup%of(j)
            spec => extension_ptr%ptr

            src_aspect => spec%get_aspect(aspect_ids(i), _RC)
            _ASSERT(associated(src_aspect),'aspect '// aspect_ids(i)%to_string() // ' not found')

            if (src_aspect%needs_extension_for(dst_aspect)) cycle
            call new_subgroup%push_back(extension_ptr)

         end do
         if (new_subgroup%size() == 0) exit
         subgroup = new_subgroup
         
      end do

      extension_ptr = subgroup%front()
      closest_extension => extension_ptr%ptr

      _RETURN(_SUCCESS)
   end function find_closest_spec

   subroutine merge(this, other)
      class(ExtensionFamily), target, intent(inout) :: this
      type(ExtensionFamily), target, intent(in) :: other

      integer :: i, j
      type(StateItemSpecPtr) :: extension, other_extension

      outer: do i = 1, other%num_variants()
         other_extension = other%specs%of(i)

         do j = 1, this%num_variants()
            extension = this%specs%of(j)
            if (associated(extension%ptr, other_extension%ptr)) cycle outer
         end do
         call this%specs%push_back(other_extension)
         
      end do outer
      this%has_primary_ = other%has_primary_

   end subroutine merge

   logical function is_deferred(this, rc)
      class(ExtensionFamily), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemSpec), pointer :: primary

      is_deferred = .false.
      primary => this%get_primary(_RC)
      is_deferred = primary%has_deferred_aspects()
      
      _RETURN(_SUCCESS)
   end function is_deferred

   ! Wrapper that returns the primary spec directly
   function get_primary_spec(this, rc) result(spec)
      type(StateItemSpec), pointer :: spec
      class(ExtensionFamily), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      spec => this%get_primary(_RC)

      _RETURN(_SUCCESS)
   end function get_primary_spec

end module mapl3g_ExtensionFamily


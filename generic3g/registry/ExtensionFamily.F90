#include "MAPL.h"

! A StateItem can be extended by means of a coupler.  The
! set of all such related extensions are encapsulated
! in objects of type ExtensionFamily.


module mapl3g_ExtensionFamily
   use mapl3g_StateItemSpec
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_StateItemExtension
   use mapl3g_StateItemExtensionPtrVector
   use mapl_ErrorHandling
   use gFTL2_StringVector
   implicit none
   private

   public :: ExtensionFamily

   ! The primary/base item spec is tracked separately to enable
   ! control of which will appear in user states with its short-name.
   type :: ExtensionFamily
      private
      logical :: has_primary_ = .false.
      type(StateItemExtensionPtrVector) :: extensions
   contains
      procedure :: has_primary
      procedure :: get_primary
      procedure :: get_extensions
      procedure :: get_extension
      procedure :: add_extension
      procedure :: num_variants
      procedure :: merge

      procedure :: find_closest_extension
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
      type(StateItemExtension), pointer, intent(in) :: primary

      type(StateItemExtensionPtr) :: wrapper

      family%has_primary_ = .true.
      wrapper%ptr => primary
      call family%extensions%push_back(wrapper)

   end function new_ExtensionFamily_primary

   logical function has_primary(this)
      class(ExtensionFamily), intent(in) :: this
      has_primary = this%has_primary_
   end function has_primary

   function get_primary(this, rc) result(primary)
      type(StateItemExtension), pointer :: primary
      class(ExtensionFamily), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      type(StateItemExtensionPtr), pointer :: wrapper

      primary => null()
      _ASSERT(this%has_primary_, "No primary item spec")
      _ASSERT(this%extensions%size() > 0, "No primary item spec")
      wrapper => this%extensions%front()
      primary => wrapper%ptr
      _RETURN(_SUCCESS)
   end function get_primary

   function get_extensions(this) result(extensions)
      type(StateItemExtensionPtrVector), pointer :: extensions
      class(ExtensionFamily), target, intent(in) :: this
      extensions => this%extensions
   end function get_extensions

   function get_extension(this, i) result(extension)
      type(StateItemExtension), pointer :: extension
      integer, intent(in) :: i
      class(ExtensionFamily), target, intent(in) :: this

      type(StateItemExtensionPtr), pointer :: wrapper
      wrapper => this%extensions%at(i)
      extension => wrapper%ptr
   end function get_extension

   subroutine add_extension(this, extension)
      class(ExtensionFamily), intent(inout) :: this
      type(StateItemExtension), pointer, intent(in) :: extension

      type(StateItemExtensionPtr) :: wrapper

      wrapper%ptr => extension
      call this%extensions%push_back(wrapper)

   end subroutine add_extension

   integer function num_variants(this)
      class(ExtensionFamily), intent(in) :: this
      num_variants = this%extensions%size()
   end function num_variants


   function find_closest_extension(family, goal_spec, rc) result(closest_extension)
      type(StateItemExtension), pointer :: closest_extension
      class(ExtensionFamily), intent(in) :: family
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(StateItemExtensionPtrVector) :: subgroup, new_subgroup
      class(StateItemSpec), pointer :: archetype
      integer :: i, j
      integer :: status
      type(StateItemExtensionPtr) :: extension_ptr
      type(StateItemExtension), pointer :: primary
      class(StateItemSpec), pointer :: spec
      logical :: match
      type(AspectId), allocatable :: aspect_ids(:)

      class(StateItemAspect), pointer :: src_aspect, dst_aspect

      closest_extension => null()
      subgroup = family%get_extensions()
      primary => family%get_primary()  ! archetype defines the rules
      archetype => primary%get_spec()
      ! new
      aspect_ids = archetype%get_aspect_order(goal_spec)
      do i = 1, size(aspect_ids)
         dst_aspect => goal_spec%get_aspect(aspect_ids(i), _RC)
         _ASSERT(associated(dst_aspect), 'expected aspect '// aspect_ids(i)%to_string() //' is missing')

         ! Find subset that match current aspect
         new_subgroup = StateItemExtensionPtrVector()
         do j = 1, subgroup%size()
            extension_ptr = subgroup%of(j)
            spec => extension_ptr%ptr%get_spec()

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
   end function find_closest_extension

   subroutine merge(this, other)
      class(ExtensionFamily), target, intent(inout) :: this
      type(ExtensionFamily), target, intent(in) :: other

      integer :: i, j
      type(StateItemExtensionPtr) :: extension, other_extension

      outer: do i = 1, other%num_variants()
         other_extension = other%extensions%of(i)

         do j = 1, this%num_variants()
            extension = this%extensions%of(j)
            if (associated(extension%ptr, other_extension%ptr)) cycle outer
         end do
         call this%extensions%push_back(other_extension)
         
      end do outer
      this%has_primary_ = other%has_primary_

   end subroutine merge
   
end module mapl3g_ExtensionFamily


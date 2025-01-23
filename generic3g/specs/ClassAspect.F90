#include "MAPL_Generic.h"

module mapl3g_ClassAspect
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl_ErrorHandling
   implicit none
   private

   public :: ClassAspect
   public :: to_ClassAspect ! cast from poly

   interface to_ClassAspect
      procedure :: to_class_from_poly
      procedure :: to_class_from_map
   end interface to_ClassAspect

   type, abstract, extends(StateItemAspect) :: ClassAspect
   contains
      procedure(I_get_aspect_order), deferred :: get_aspect_order
      procedure(I_create), deferred :: create
      procedure(I_destroy), deferred :: destroy
      procedure(I_allocate), deferred :: allocate

      procedure(I_add_to_state), deferred :: add_to_state
      procedure(I_add_to_bundle), deferred :: add_to_bundle
      procedure, non_overridable, nopass :: get_aspect_id
   end type ClassAspect

   abstract interface

      function I_get_aspect_order(this, goal_aspects) result(aspect_ids)
         use mapl3g_StateItemAspect
         import ClassAspect, AspectId
         type(AspectId), allocatable :: aspect_ids(:)
         class(ClassAspect), intent(in) :: this
         type(AspectMap), intent(in) :: goal_aspects
      end function I_get_aspect_order

      ! Will use ESMF so cannot be PURE
      subroutine I_create(this, rc)
         import ClassAspect
         class(ClassAspect), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_create

      subroutine I_destroy(this, rc)
         import ClassAspect
         class(ClassAspect), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_destroy

      ! Will use ESMF so cannot be PURE
      subroutine I_allocate(this, other_aspects, rc)
         import ClassAspect
         import AspectMap
         class(ClassAspect), intent(inout) :: this
         type(AspectMap), intent(in) :: other_aspects
         integer, optional, intent(out) :: rc
      end subroutine I_allocate

      subroutine I_add_to_state(this, multi_state, actual_pt, rc)
         use mapl3g_MultiState
         use mapl3g_ActualConnectionPt
         import ClassAspect
         class(ClassAspect), intent(in) :: this
         type(MultiState), intent(inout) :: multi_state
         type(ActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
      end subroutine I_add_to_state

      subroutine I_add_to_bundle(this, field_bundle, rc)
         use ESMF, only: ESMF_FieldBundle
         import ClassAspect
         class(ClassAspect), intent(in) :: this
         type(ESMF_FieldBundle), intent(inout) :: field_bundle
         integer, optional, intent(out) :: rc
      end subroutine I_add_to_bundle

   end interface

contains

   function to_class_from_poly(aspect, rc) result(class_aspect)
      class(ClassAspect), allocatable :: class_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type(aspect)
      class is (ClassAspect)
         class_aspect = aspect
      class default
         _FAIL('aspect is not ClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_class_from_poly

   function to_class_from_map(map, rc) result(class_aspect)
      class(ClassAspect), allocatable :: class_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(CLASS_ASPECT_ID, _RC)
      class_aspect = to_ClassAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_class_from_map
   
   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id


end module mapl3g_ClassAspect

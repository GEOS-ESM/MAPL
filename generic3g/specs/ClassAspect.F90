#include "MAPL_Generic.h"

module mapl3_ClassAspect
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
      procedure, nopass :: get_aspect_id
   end type ClassAspect

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


end module mapl3_ClassAspect

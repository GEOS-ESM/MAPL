module mapl3_ClassAspect
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   implicit none
   private

   public :: ClassAspect

   type, abstract, extends(StateItemAspect) :: ClassAspect
   contains
      procedure, nopass :: get_aspect_id
   end type ClassAspect

contains

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

end module mapl3_ClassAspect

#include "MAPL.h"
module mapl_FieldNode
   use mapl_StateItemNode
   use esmf
   implicit none (type,external)
   private

   public :: FieldNode

   type, extends(StateItemNode) :: FieldNode
      private
      type(esmf_Field) :: payload
   contains

   end type FieldNode

   interface FieldNode
      interface new_FieldNode
   end interface FieldNode

contains

   function new_FieldNode()
      
   end function new_FieldNode

   subroutine get_payload(this, unusuable, geom, field, bundle, state, rc)
         class(FieldNode), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(esmf_Geom), optional, allocatable, intent(out) :: geom
         type(esmf_Field), optional, allocatable, intent(out) :: field
         type(esmf_FieldBundle), optional, allocatable, intent(out) :: bundle
         type(esmf_State), optional, allocatable, intent(out) :: state
         integer, optional, intent(out) :: rc

         field = this%payload
         
         _RETURN(_SUCCESS)
   end subroutine get_payload
   
end module mapl_FieldNode

#include "MAPL.h"
module mapl_StateNode
   use mapl_StateItemNode
   use esmf
   implicit none (type,external)
   private

   public :: StateNode

   type, extends(StateItemNode) :: StateNode
      private
      type(esmf_State) :: payload
      type(StringNodeIdMap) :: items
   contains

   end type StateNode


contains

   subroutine get_payload(this, unusuable, geom, field, bundle, state, rc)
         class(StateNode), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(esmf_Geom), optional, allocatable, intent(out) :: geom
         type(esmf_Field), optional, allocatable, intent(out) :: field
         type(esmf_FieldBundle), optional, allocatable, intent(out) :: bundle
         type(esmf_State), optional, allocatable, intent(out) :: state
         integer, optional, intent(out) :: rc

         state = this%payload
         
         _RETURN(_SUCCESS)
   end subroutine get_payload
   
end module mapl_StateNode

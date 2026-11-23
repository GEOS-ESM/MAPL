!------------------------------------------------------------------------------
! StateItemKeyPrefix: the resource-index key prefix for an INTERNAL
! item (GraphBuilder.F90's item_key()), shared with graph-neutral code
! (CompositeStateMaterialization.F90) that cannot depend on
! mapl_GraphBuilder_mod (REQ-CG-002).
!------------------------------------------------------------------------------
module mapl_StateItemKeyPrefix_mod
   implicit none(type, external)
   private

   public :: INTERNAL_ITEM_KEY_PREFIX

   character(*), parameter :: INTERNAL_ITEM_KEY_PREFIX = 'INTERNAL:'

end module mapl_StateItemKeyPrefix_mod

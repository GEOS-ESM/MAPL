! Predicates over a declared item class (ESMF_StateItem_Flag). Kept apart
! from the graph builder so any layer may ask them without depending on
! OuterComponent or StateRegistry.
module mapl_ItemClassPredicates_mod
   use esmf, only: ESMF_StateItem_Flag, operator(==)
   use mapl_StateItem_mod, only: MAPL_STATEITEM_FIELD, &
        MAPL_STATEITEM_VECTOR, MAPL_STATEITEM_BRACKET
   implicit none
   private

   public :: is_graph_creatable_class

contains

   ! The item classes the graph can create a payload for: a Field, or a
   ! bundle for a vector or bracket. Every other class (SERVICE, STATE, ...)
   ! is outside this set. Not pure: ESMF's == on ESMF_StateItem_Flag is impure.
   logical function is_graph_creatable_class(item_class) result(ok)
      type(ESMF_StateItem_Flag), intent(in) :: item_class

      ok = (item_class == MAPL_STATEITEM_FIELD) .or. &
           (item_class == MAPL_STATEITEM_VECTOR) .or. &
           (item_class == MAPL_STATEITEM_BRACKET)
   end function is_graph_creatable_class

end module mapl_ItemClassPredicates_mod

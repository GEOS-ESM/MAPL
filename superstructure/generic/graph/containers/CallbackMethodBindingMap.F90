!------------------------------------------------------------------------------
! CallbackMethodBindingMap: gFTL map of a compound string key (built by
! GraphBuilder.F90's callback_method_binding_key(), member qualified name
! + method name) -> CallbackMethodBinding. Stored on a callback
! collection's own StateItemNode (the node materialize_callback_collection
! builds) so an Invoker can reach every matched member's bound methods
! without re-deriving them. A compound string key is used instead of a
! map-of-maps, following the same keying style as GraphBuilder.F90's own
! item_key()/'CALLBACK_COLLECTION:' reuse-entry keys.
!------------------------------------------------------------------------------
module mapl_CallbackMethodBindingMap_mod
   use mapl_CallbackMethodBinding_mod, only: CallbackMethodBinding

#define Key __CHARACTER_DEFERRED
#define T CallbackMethodBinding
#define Map CallbackMethodBindingMap
#define MapIterator CallbackMethodBindingMapIterator
#define Pair CallbackMethodBindingPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackMethodBindingMap_mod

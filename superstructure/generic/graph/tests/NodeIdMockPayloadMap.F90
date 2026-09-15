!------------------------------------------------------------------------------
! Test-only gFTL map instantiation: NodeId-keyed, polymorphic-valued map,
! demonstrating REQ-ID-006 (spec/05-identities.md) directly - NodeId is
! usable as a gFTL map key for a polymorphic mapped value with no Box
! wrapper type, using the same Key/Key_LT/T/T_polymorphic pattern MAPL
! itself uses (superstructure/generic/graph/containers/NodeMap.F90).
!------------------------------------------------------------------------------
module NodeIdMockPayloadMap_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)
   use MockPayload_mod, only: MockPayload

#define Key NodeId
#define Key_LT(a,b) (a < b)
#define T MockPayload
#define T_polymorphic
#define Map NodeIdMockPayloadMap
#define MapIterator NodeIdMockPayloadMapIterator
#define Pair NodeIdMockPayloadPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key_LT
#undef Key

end module NodeIdMockPayloadMap_mod

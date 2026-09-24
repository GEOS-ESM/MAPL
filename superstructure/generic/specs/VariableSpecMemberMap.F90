!------------------------------------------------------------------------------
! VariableSpecMemberMap: gFTL map (string -> polymorphic VariableSpecTag),
! used by VariableSpec for its own declared-member storage
! (openspec/changes/composite-state-spec). T is polymorphic purely to let
! VariableSpec contain a map of itself without a circular module
! dependency (design.md Decisions) - not for open extensibility
! (VariableSpec is the only concrete extension of VariableSpecTag that
! will ever exist). Same shape as containers/NodeIdGraphNodeMap.F90 (a
! polymorphic map over a foreign, already-defined abstract type, via the
! single map/template.inc include) - not the header.inc/public.inc/
! specification.inc breakdown Characteristic.F90/StateItemAspect.F90 use,
! since those modules also define their abstract T type inline; this one
! does not.
!------------------------------------------------------------------------------
module mapl_VariableSpecMemberMap_mod
   use mapl_VariableSpecTag_mod, only: VariableSpecTag

#define Key __CHARACTER_DEFERRED
#define T VariableSpecTag
#define T_polymorphic
#define Map VariableSpecMemberMap
#define MapIterator VariableSpecMemberMapIterator
#define Pair VariableSpecMemberPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl_VariableSpecMemberMap_mod

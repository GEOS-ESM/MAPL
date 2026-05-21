module mapl_GriddedComponentDriverMap
   use mapl_GriddedComponentDriver

#define Key __CHARACTER_DEFERRED
#define T GriddedComponentDriver
#define OrderedMap GriddedComponentDriverMap
#define OrderedMapIterator GriddedComponentDriverMapIterator
#define Pair GriddedComponentDriverPair

#include "ordered_map/template.inc"

#undef Pair
#undef OrderedMapIterator
#undef OrderedMap
#undef T
#undef Key

end module mapl_GriddedComponentDriverMap

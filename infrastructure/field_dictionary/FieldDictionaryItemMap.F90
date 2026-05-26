module mapl_FieldDictionaryItemMap_mod
   use mapl_FieldDictionaryItem_mod

#define Key __CHARACTER_DEFERRED
#define T FieldDictionaryItem
#define Map FieldDictionaryItemMap
#define MapIterator FieldDictionaryItemMapIterator
#define Pair FieldDictionaryItemPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_FieldDictionaryItemMap_mod

module mapl_DependencyNetworkMap_mod
   use mapl_DependencyNetworkId_mod
   use mapl_DependencyNetwork_mod

#define Key DependencyNetworkId
#define Key_LT(a,b) (a < b)
#define T DependencyNetwork
#define Map DependencyNetworkMap
#define MapIterator DependencyNetworkMapIterator
#define Pair DependencyNetworkMapPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_DependencyNetworkMap_mod




  

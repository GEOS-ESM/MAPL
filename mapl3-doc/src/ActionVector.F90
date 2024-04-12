module mapl3g_ActionVector
   use mapl3g_ExtensionAction

#define T ExtensionAction
#define T_polymorphic
#define Vector ActionVector
#define VectorIterator ActionVectorIterator

#include "vector/template.inc"

#undef T
#undef T_polymorphic
#undef Vector
#undef VectorIterator

end module mapl3g_ActionVector


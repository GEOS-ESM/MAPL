module mapl3g_ActionSequence
   use mapl3g_ExtensionAction

#define T ExtensionAction
#define T_polymorphic
#define Vector ActionSequence
#define VectorIterator ActionSequenceIterator

#include "vector/template.inc"

#undef T
#undef T_polymorphic
#undef Vector
#undef VectorIterator

end module mapl3g_ActionSequence


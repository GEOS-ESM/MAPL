module sf_ElementVector
   use sf_Element

#define T Element
#define Vector ElementVector
#define VectorIterator ElementVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module sf_ElementVector

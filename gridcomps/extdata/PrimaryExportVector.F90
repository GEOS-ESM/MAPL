module mapl_PrimaryExportVector
   use mapl_PrimaryExport
#define T PrimaryExport
#define Vector PrimaryExportVector
#define VectorIterator PrimaryExportVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_PrimaryExportVector

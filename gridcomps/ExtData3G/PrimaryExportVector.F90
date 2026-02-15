module mapl3g_PrimaryExportVector
   use mapl3g_PrimaryExport
#define T PrimaryExport
#define Vector PrimaryExportVector
#define VectorIterator PrimaryExportVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl3g_PrimaryExportVector

module mapl_PrimaryExportVector_mod
   use mapl_PrimaryExport_mod
#define T PrimaryExport
#define Vector PrimaryExportVector
#define VectorIterator PrimaryExportVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_PrimaryExportVector_mod

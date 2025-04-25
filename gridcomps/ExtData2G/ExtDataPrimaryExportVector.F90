module MAPL_ExtDataPrimaryExportVectorMod
   use MAPL_ExtDataTypeDef
#define T PrimaryExport
#define Vector PrimaryExportVector
#define VectorIterator PrimaryExportVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module MAPL_ExtDataPrimaryExportVectorMod

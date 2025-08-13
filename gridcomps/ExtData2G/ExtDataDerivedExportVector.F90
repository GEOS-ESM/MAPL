module MAPL_ExtDataDerivedExportVectorMod
   use MAPL_ExtDataTypeDef
#define T DerivedExport
#define Vector DerivedExportVector
#define VectorIterator DerivedExportVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module MAPL_ExtDataDerivedExportVectorMod

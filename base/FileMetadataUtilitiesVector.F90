module MAPL_FileMetadataUtilsVectorMod
   use MAPL_FileMetadataUtilsMod

#define T FileMetadataUtils
#define Vector FileMetadataUtilsVector
#define VectorIterator FileMetadataUtilsVectorIterator
#define VectorRIterator FileMetadataUtilsVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T

end module MAPL_FileMetadataUtilsVectorMod

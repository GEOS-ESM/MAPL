module pFIO_FileMetadataVectorMod
   use pFIO_FileMetadataMod

#define T FileMetadata
#define Vector FileMetaDataVector
#define VectorIterator FileMetadataVectorIterator
#define VectorRIterator FileMetadataVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T

end module pFIO_FileMetadataVectorMod

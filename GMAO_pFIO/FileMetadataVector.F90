module pFIO_FileMetadataVectorMod
   use pFIO_FileMetadataMod

#define _type type (FileMetadata)
#define _vector FileMetadataVector
#define _iterator FileMetadataVectorIterator
#include "templates/vector.inc"

end module pFIO_FileMetadataVectorMod

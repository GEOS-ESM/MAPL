module mapl3g_DataCollectionVector
   use pFIO
   use mapl3g_DataCollection
   
   ! Create a map (associative array) between names and pFIO_Attributes.
   
#define _type type (DataCollection)
#define _vector DataCollectionVector
#define _iterator DataCollectionVectorIterator

#define _FTL_THROW pFIO_throw_exception

#include "templates/vector.inc"
   
end module mapl3g_DataCollectionVector


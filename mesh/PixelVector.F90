module sf_PixelVector
   use sf_Pixel

#define T Pixel
#define Vector PixelVector
#define VectorIterator PixelVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module sf_PixelVector

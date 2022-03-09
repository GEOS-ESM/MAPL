module MAPL_ExtDataNG_IOBundleVectorMod
  use MAPL_ExtDataNG_IOBundleMod

#define T ExtDataNG_IoBundle
#define Vector IoBundleNGVector
#define VectorIterator IoBundleNGVectorIterator
#include "vector/template.inc"
#undef VectorIterator
#undef Vector
#undef T
  
end module MAPL_ExtDataNG_IOBundleVectorMod

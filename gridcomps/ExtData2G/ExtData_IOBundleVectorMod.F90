module MAPL_ExtDataNG_IOBundleVectorMod
  use MAPL_ExtDataNG_IOBundleMod
  
#define _type type(ExtDataNG_IoBundle)
#define _vector IoBundleNGVector
#define _iterator IoBundleNGVectorIterator

#include "templates/vector.inc"

end module MAPL_ExtDataNG_IOBundleVectorMod

module MAPL_ExtData_IOBundleVectorMod
  use MAPL_ExtData_IOBundleMod
  
#define T ExtData_IoBundle 
#define Vector IoBundleVector
#define VectorIterator IoBundleVectorIterator
#define VectorRIterator IoBundleVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T

end module MAPL_ExtData_IOBundleVectorMod

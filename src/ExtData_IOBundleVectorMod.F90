module MAPL_ExtData_IOBundleVectorMod
  use MAPL_ExtData_IOBundleMod
  
#define _type type(ExtData_IoBundle)
#define _vector IoBundleVector
#define _iterator IoBundleVectorIterator

#include "templates/vector.inc"

end module MAPL_ExtData_IOBundleVectorMod

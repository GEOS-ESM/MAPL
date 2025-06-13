#include "undef_macros.h"
#define _ISIN(V) findloc(_SET, spec%V) >= lbound(_SET)
#define _ISIN_STRINGVECTOR(V) is_stringvector_subset(V, _SET)
#define _ISIN_RANGES(V) is_in(V, _SET)

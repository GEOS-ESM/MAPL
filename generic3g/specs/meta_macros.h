#include "undef_macros.h"
#define _ISIN(V) findloc(_SET, spec%V) >= lbound(_SET)
#define _ISIN_STRINGVECTOR(V) is_stringvector_subset(V, _SET)
#define _ISIN_RANGES(V) is_in(V, _SET)

#define _ISIN_(V, S) findloc(S, spec%V) >= lbound(S)
#define _VECTOR_ISIN_VECTOR_(V1, V2) is_stringvector_subset(V1, V2)
#define _ISIN_RANGES_(V, R) is_in(V, R)

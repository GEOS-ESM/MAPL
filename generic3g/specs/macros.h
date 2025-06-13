#include "meta_undef_macros"
#include "meta_macros.h"
#define _INVALID(V) "Invalid " // #V
#define _ASSERT_SPEC_VALUE__(V, F) _ASSERT(F(spec%V), _INVALID(V))
#define _ASSERT_SPEC_VALUE_(P, V, F) P=present(spec%V); if(P) then; _ASSERT_SPEC_VALUE__(V, F)
#define _ASSERT_SPEC_VALUE(P, V, F) _ASSERT_SPEC_VALUE_(P, V, F); P=.FALSE.
#define _ASSERT_IN_SET(P, V, _SET) _ASSERT_SPEC_VALUE(P, V, _ISIN)
#define _ASSERT_VALID_STRINGVECTOR(P, V, _SET) _ASSERT_SPEC_VALUE(P, V, _ISIN_STRINGVECTOR)
#define _ASSERT_IN_RANGES(P, V, _SET) _ASSERT_SPEC_VALUE(P, V, _ISIN_RANGE)
#define _ASSERT_EITHER_SPEC_VALUE_(P, V1, F1, V2, F2) _ASSERT_SPEC_VALUE_(P, V1, F1);\
    if(.not. P) then; _ASSERT_SPEC_VALUE_(P, V2, F2)
#define _ASSERT_EITHER_SPEC_VALUE(P, V1, F1, V2, F2) _ASSERT_EITHER_SPEC_VALUE_(P, V1, F1, V2, F2); P=.FALSE.

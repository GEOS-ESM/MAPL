#include "undef_macros.h"
#if !defined(_SPEC_)
#   define _SPEC_ spec
#endif
#define _SPEC(V) _SPEC_ ## % ## V
#define _ALLOCATED(V) allocated(_SPEC(V))
#define _INVALID(V) "Invalid " // #V
#define _ASSERT_LOGICAL(FV, V) _ASSERT(FV, _INVALID(V))
#define _ASSERT_FUNCTION_(F, V)  _ASSERT_LOGICAL(F(_SPEC(V)), V)
#define _ASSERT_FUNCTION(F, V) if(_ALLOCATED(V)) then; _ASSERT_FUNCTION_(F, V); end if
#define _ASSERT_IS_(V1, V2) _ASSERT_LOGICAL(_SPEC(V2) == V1, V2)
#define _ASSERT_IS(V1, V2) if(_ALLOCATED(V1)) then; _ASSERT_IS_(V1, V2); end if
#define _ASSERT_FUNCTIONS(F1, V1, F2, V2) if(_ALLOCATED(V1)) then;_ASSERT_FUNCTION_(F1, V1); else if(_ALLOCATED(V2)) then; _ASSERT_FUNCTION_(F2, V2); end if
#define _ASSERT_PARAM_FUNC_(F, P, V) _ASSERT_LOGICAL(F(P, _SPEC(V)), V)
#define _ASSERT_PARAM_FUNC(F, P, V) if(_ALLOCATED(V)) then; _ASSERT_PARAM_FUNC_(F, P, V); end if

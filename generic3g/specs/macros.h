#include "undef_macros.h"
#if defined(_SPEC_)
#   define _SPEC(V) _SPEC_ ## % ## V
#else
#   define _SPEC(V) spec ## % ## V
#endif
#define _ALLOCATED(V) allocated(_SPEC(V))


_ASSERT(_ALLOCATED_(state_intent, valid_state_intent), "message")
#define _ASSERT_FUNCTION(F, V, M) if(allocated(_SPEC(V))) then; _ASSERT_FUNCTION_(F, V, M); end if
#define _ASSERT_IS_(V1, V2, M) _ASSERT_LOGICAL(_SPEC(V2) == V1, V2, M)
#define _ASSERT_IS(V1, V2, M) if(_ALLOCATED(V1)) then; _ASSERT_IS_(V1, V2, M); end if

#define _ASSERT_FUNCTIONS(F1, V1, F2, V2, M) if(_ALLOCATED(V1)) then;_ASSERT_FUNCTION_(F1, V1); else if(_ALLOCATED(V2)) then; _ASSERT_FUNCTION_(F2, V2); end if
#define _ASSERT_PARAM_FUNC_(F, P, V) _ASSERT_LOGICAL(F(P, _SPEC(V)), V)
#define _ASSERT_PARAM_FUNC(F, P, V) if(_ALLOCATED(V)) then; _ASSERT_PARAM_FUNC_(F, P, V); end if

#define _EVAL_IF(V, F) if _ALLOCATED(V); F(V) 

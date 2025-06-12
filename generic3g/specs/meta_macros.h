#include "undef_macros.h"
#define _INVALID(V) "Invalid " // #V
#define _ASSERT_SPEC_VALUE(V, F) if(present(spec%V)) then;_ASSERT(F(spec%V), _INVALID(V));end if
#define _ISIN(V) findloc(_SET, spec%V) >= lbound(_SET)

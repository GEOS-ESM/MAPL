#include "meta_undef_macros"
#include "meta_macros.h"
#define _ASSERT_VALUE_IN(V, B) if(present(spec%V)) then;_ASSERT(is_in(spec%V, B), _INVALID(V));end if 
#define _ASSERT_VALID_STRINGVECTOR(V1, V2) if(present(spec%V)) then;_ASSERT(valid_string_vector(spec%V1, V2), _INVALID(V1));end if
#define _ASSERT_IN_SET(V, _SET) _ASSERT_SPEC(V, _ISIN, _INVALID(V));end if

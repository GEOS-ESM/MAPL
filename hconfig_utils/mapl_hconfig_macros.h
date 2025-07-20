#ifndef _MAPL_HCONFIG_MACROS_

#include "mapl_hconfig_macros_undef.h"

#define _VAL_ val
#define _DEFAULT_ default
#define _EQ_ =
#define _OPEN_ (
#define _CLOSE_ )
#define _COLON_ :
#define _DIMS_ _OPEN_ ## _COLON_ ## _CLOSE_

#define _INT_ 0
#define _REAL_ 1
#define _LOGICAL_ 2
#define _STRING_ 4
#define _SEQ_ Seq
#define _EDIT_DESC_DEF_ G0

#define _CAT2(A, B) A ## B
#define _CAT3(A, B, C) A ## B ## C
#define _QUOTE(A) _STR(A)
#define _STR(A) #A
#define _PFTYPE(T, L, V) T ## _OPEN_ ## L ## _EQ_ ## V ## _CLOSE_
#define _ESMF_KIND(TK) ESMF_KIND_ ## TK

#define _MAPL_HCONFIG_MACROS_
 
#endif

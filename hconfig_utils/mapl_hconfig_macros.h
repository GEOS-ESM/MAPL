#ifndef _MAPL_HCONFIG_MACROS_

#include "mapl_hconfig_macros_undef.h"
#define _CAT(A) A
#define _CAT2(A, B) A ## B
#define _CAT3(A, B, C) _CAT2(A,B) ## C
#define _CAT4(A, B, C, D) _CAT2(A,B) ## _CAT2(C,D)
#define _CAT5(A, B, C, D, E) _CAT3(A, B, C) ## _CAT2(D, E)
#define _OPEN_ (
#define _CLOSE_ )
#define _ALLOCATABLE_ , allocatable
#define _ASSUMED_LEN_STRING_ character(len=*)
#define _ALLOCATABLE_STRING_ character(len=:) ## _ALLOCATABLE_
#define _ARRAY_DIMS_ (:)

#define _MAPL_HCONFIG_MACROS_

#endif

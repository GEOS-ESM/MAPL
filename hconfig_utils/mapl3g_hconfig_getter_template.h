#include "mapl3g_hconfig_getter_macros.h"

#define FMT_ G0
#if (TYPE_==TYPEI4)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4
#   define TYPESTRING_ I4
#elif (TYPE_==TYPECH)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsString
#   define VALTYPE character(len=:), allocatable
#   define TYPESTRING_ CH
#endif

#if !defined VALTYPE
#   define VALTYPE TYPE_
#endif

#if !defined RELOP
#   define RELOP ==
#endif

#if defined IS_ARRAY
#   define RELFCT(A, B) all(A RELOP B)
#   define VALTYPE VALTYPE, dimension(:), allocatable
#   define FMTSTR '([ FMT_, *(", ", FMT_)])'
#else
#   define RELFCT(A, B) A RELOP B
#   define FMTSTR '(FMT_)'
#endif

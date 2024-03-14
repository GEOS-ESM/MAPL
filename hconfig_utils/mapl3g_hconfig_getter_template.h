#include "mapl3g_hconfig_getter_macros.h"

#if TYPE_ == integer(kind=ESMF_KIND_I4)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4
#   define TYPESTRING_ I4
#elif TYPE_ == character(len=*)
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

#if !defined FMT
#   define FMT G0
#endif

#if defined IS_ARRAY
#   define RELFCT(A, B) all(A RELOP B)
#   define VALTYPE VALTYPE, dimension(:), allocatable
#   define FMTSTR '([ FMT, *(", ", FMT)])'
#else
#   define RELFCT(A, B) A RELOP B
#   define FMTSTR '(FMT)'
#endif

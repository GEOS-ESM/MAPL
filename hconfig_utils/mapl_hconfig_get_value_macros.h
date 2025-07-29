#ifndef _MAPL_HCONFIG_MACROS_
#include "mapl_hconfig_macros.h"
#endif

#ifdef _IS_LOGICAL_
#  define _RELATION(V, D) V .eqv. D
#endif

#ifndef _RELATION
#  define _RELATION(V, D) V == D
#endif

#ifdef _VTYPE_
#   undef _VTYPE_
#endif
#ifdef _DTYPE_
#   undef _DTYPE_
#endif

#ifdef _IS_CHARACTER_
#  define _VTYPE_ _ALLOCATABLE_STRING_
#  define _DTYPE_ _ASSUMED_LEN_STRING_
#endif

#ifdef _ARRAY_
#  ifndef _IS_CHARACTER_
#     define _VTYPE_ _FTYPE_, allocatable
#     define _DTYPE_ _FTYPE_
#  endif
#  define _DIMS_ _ARRAY_DIMS_
#  define _DECL_NUM_ITEMS_ integer :: num_items
#  define _SET_NUM_ITEMS(N, V) N = min(size(V), MAX_NUM_ITEMS_OUTPUT) 
#  define _COMPARE(V, D) all( _RELATION(V, D) )
#  define _WRITE_DIMS_ (1:num_items)
#  define _ADJUST_VALUESTRING(S, V) if(size(V)>num_items) S=S//ELLIPSIS; S='['//S//']'
#else
#  ifndef _IS_CHARACTER_
#       define _VTYPE_ _FTYPE_
#       define _DTYPE_ _FTYPE_
#  endif
#  define _DIMS_
#  define _DECL_NUM_ITEMS_
#  define _SET_NUM_ITEMS(N, V)
#  define _COMPARE(V, D) ( _RELATION(V, D) )
#  define _WRITE_DIMS_
#  define _ADJUST_VALUESTRING(S, V) 
#endif

#undef _RELATION

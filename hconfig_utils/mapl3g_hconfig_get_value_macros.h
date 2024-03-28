! vim:ft=fortran
#include "mapl3g_hconfig_macro_init.h"

#define GET_VALUE_ SUBROUTINE_NAME

#if (TYPENUM==TYPEI4)
#   define DEFTYPE integer(kind=ESMF_KIND_I4)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4
#   define TYPESTRING_ 'I4'
#elif (TYPENUM==TYPEL)
#   define DEFTYPE logical
#   define ESMF_HCONFIG_AS ESMF_HConfigAsLogical
#   define IS_LOGICAL
#elif (TYPENUM==TYPEI4SEQ)
#   define DEFTYPE integer(kind=ESMF_KIND_I4)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4Seq
#   define RANK_ (:) 
#   define TYPESTRING_ 'I4'
#elif (TYPENUM==TYPEL_SEQ)
#   define DEFTYPE logical
#   define ESMF_HCONFIG_AS ESMF_HConfigAsLogicalSeq
#   define RANK_ (:) 
#   define IS_LOGICAL
#elif (TYPENUM==TYPECH)
#   define DEFTYPE character(len=*)
#   define VALTYPE character(len=:), allocatable
#   define ESMF_HCONFIG_AS ESMF_HConfigAsString
#endif

#if define IS_LOGICAL
#  define RELATIONAL_OPERATOR .eqv.
#else
#  define RELATIONAL_OPERATOR ==
#endif

#if defined RANK_
#  define VALTYPE DEFAULT, allocatable
#  define RELATION(A, B) all(A RELATIONAL_OPERATOR B)
#else
#  if !defined VALTYPE
#     define VALTYPE DEFTYPE
#  endif
#  define RELATION(A, B) A RELATIONAL_OPERATOR B
#  define RANK_ ! SCALAR
#endif

#if !defined FMT_
#   define FMT_ 'G0:", "'
#endif

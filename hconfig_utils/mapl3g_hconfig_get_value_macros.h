! vim:ft=fortran

#define GET_VALUE_ SUBROUTINE_NAME

#if (TYPENUM==TYPEI4)
#   define DEFTYPE integer(kind=ESMF_KIND_I4)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4
#   define TYPESTRING_ 'I4'
#elif (TYPENUM==TYPEL)
#   define DEFTYPE logical
#   define ESMF_HCONFIG_AS ESMF_HConfigAsLogical
#   define TYPESTRING_ 'L'
#   define RELATION(A, B) A.eqv.B
#elif (TYPENUM==TYPEI4SEQ)
#   define DEFTYPE integer(kind=ESMF_KIND_I4)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4Seq
#   define TYPESTRING_ 'I4'
#   define IS_ARRAY
#   define RANK_ (:) 
#elif (TYPENUM==TYPEL_SEQ)
#   define DEFTYPE logical
#   define ESMF_HCONFIG_AS ESMF_HConfigAsLogicalSeq
#   define TYPESTRING_ 'L'
#   define RELATION(A, B) all(A.eqv.B)
#   define IS_ARRAY
#   define RANK_ (:) 
#elif (TYPENUM==TYPECH)
#   define DEFTYPE character(len=*)
#   define VALTYPE character(len=*)
#   define ESMF_HCONFIG_AS ESMF_HConfigAsString
#   define TYPESTRING_ 'CH'
#endif

#if !defined RANK_
#  define RANK_ ! SCALAR
#endif

#if !defined RELATION
#   if defined IS_ARRAY
#       define RELATION(A, B) all(A==B)
#   else
#       define RELATION(A, B) (A==B)
#   endif
#endif

#if !defined FMT_
#   define FMT_ 'G0:", "'
#endif

#if !defined VALTYPE
#  define VALTYPE DEFTYPE
#endif

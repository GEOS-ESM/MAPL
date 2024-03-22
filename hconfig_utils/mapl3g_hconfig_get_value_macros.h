#define GET_VALUE_ SUBROUTINE_NAME

#if (TYPENUM==TYPEI4)
#   define DEFTYPE integer(kind=ESMF_KIND_I4)
#   define VALTYPE DEFTYPE 
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4
#   define TYPESTRING_ 'I4'
#   define RELATION(A, B) A==B
#   define FMT_ 'G0:", "'
#elif (TYPENUM==TYPEI4SEQ)
#   define DEFTYPE integer(kind=ESMF_KIND_I4), dimension(:)
#   define VALTYPE DEFTYPE, allocatable
#   define ESMF_HCONFIG_AS ESMF_HConfigAsI4Seq
#   define TYPESTRING_ 'I4'
#   define RELATION(A, B) all(A==B)
#   define FMT_ 'G0:", "'
#elif (TYPENUM==TYPECH)
#   define DEFTYPE character(len=*)
#   define VALTYPE character(len=:), allocatable
#   define ESMF_HCONFIG_AS ESMF_HConfigAsString
#   define TYPESTRING_ 'CH'
#   define RELATION(A, B) A==B
#   define FMT_ 'G0:", "'
#endif

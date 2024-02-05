#if defined TYPE_I4
#undef TYPE_I4
#endif

#if defined TYPE_I8
#undef TYPE_I8
#endif

#if defined TYPE_R4
#undef TYPE_R4
#endif

#if defined TYPE_R8
#undef TYPE_R8
#endif

#if defined TYPE_LOGICAL
#undef TYPE_LOGICAL
#endif

#if defined TYPE_CHARACTER
#undef TYPE_CHARACTER
#endif

#define TYPE_I4 integer(kind=ESMF_KIND_I4)
#define TYPE_I8 integer(kind=ESMF_KIND_I8)
#define TYPE_R4 real(kind=ESMF_KIND_R4)
#define TYPE_R8 real(kind=ESMF_KIND_R8)
#define TYPE_LOGICAL logical
#define TYPE_CHARACTER character(len=*)

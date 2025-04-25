#if defined(TYPE_STRING)
#undef TYPE_STRING
#endif

#if defined(ARE_EQUAL)
#undef ARE_EQUAL
#endif

#if defined(FMT_)
#undef FMT_
#endif


#if (TYPENUM == TYPENUM_CHARACTER)
#define TYPE_STRING TYPE_STRING_CHARACTER
#define FMT_ "(A)"

#if defined(MAKE_STRING_FUNCTION)
#undef MAKE_STRING_FUNCTION
#endif

#if defined(IS_ARRAY)
#define ARE_EQUAL(A, B) compare_all(A, B)
#define MAKE_STRING_FUNCTION(V, SL) make_string_character_array(V, SL)
#else
#define ARE_EQUAL(A, B) A==B
#define MAKE_STRING_FUNCTION(V, SL) make_string_character(V, SL)
#endif


#elif (TYPENUM == TYPENUM_INTEGER4)
#define TYPE_STRING TYPE_STRING_INTEGER4
#define FMT_ "(I0.1)"

#if defined(IS_ARRAY)
#define ARE_EQUAL(A, B) all(A==B)
#else
#define ARE_EQUAL(A, B) A==B
#endif


#elif (TYPENUM == TYPENUM_INTEGER8)
#define TYPE_STRING TYPE_STRING_INTEGER8
#define FMT_ "(I0.1)"

#if defined(IS_ARRAY)
#define ARE_EQUAL(A, B) all(A==B)
#else
#define ARE_EQUAL(A, B) A==B
#endif


#elif (TYPENUM == TYPENUM_REAL4)
#define TYPE_STRING TYPE_STRING_REAL4
#define FMT_ "(F0.6)"

#if defined(IS_ARRAY)
#define ARE_EQUAL(A, B) all(A==B)
#else
#define ARE_EQUAL(A, B) A==B
#endif


#elif (TYPENUM == TYPENUM_REAL8)
#define TYPE_STRING TYPE_STRING_REAL8
#define FMT_ "(F0.6)"

#if defined(IS_ARRAY)
#define ARE_EQUAL(A, B) all(A==B)
#else
#define ARE_EQUAL(A, B) A==B
#endif


#elif (TYPENUM == TYPENUM_LOGICAL)
#define TYPE_STRING TYPE_STRING_LOGICAL
#define FMT_ "(L1)"

#if defined(IS_ARRAY)
#define ARE_EQUAL(A, B) all(A.eqv.B)
#else
#define ARE_EQUAL(A, B) A.eqv.B
#endif



#endif

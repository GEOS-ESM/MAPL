#if defined(FMT_)
#undef FMT_
#endif

#if defined(TYPE_STRING)
#undef TYPE_STRING
#endif

#if defined(ARE_EQUAL)
#undef ARE_EQUAL
#endif

#if defined(RELATION)
#undef RELATION
#endif

#if (TYPE_NUM == 1) 
#define TYPE_STRING TYPE_STRING_INTEGER4
#define FMT_ "(I0.1)"
#define RELATION(A, B) A==B

#elif (TYPE_NUM == 2) 
#define TYPE_STRING TYPE_STRING_INTEGER8
#define FMT_ "(I0.1)"
#define RELATION(A, B) A==B

#elif (TYPE_NUM == 3) 
#define TYPE_STRING TYPE_STRING_REAL4
#define FMT_ "(F0.6)"
#define RELATION(A, B) A==B

#elif (TYPE_NUM == 4) 
#define TYPE_STRING TYPE_STRING_REAL8
#define FMT_ "(F0.6)"
#define RELATION(A, B) A==B

#elif (TYPE_NUM == 5) 
#define TYPE_STRING TYPE_STRING_LOGICAL
#define RELATION(A, B) A.eqv.B
#define FMT_ "(L1)"

#elif (TYPE_NUM == 0) 
#define TYPE_STRING TYPE_STRING_CHARACTER
#define RELATION(A, B) A==B
#define FMT_ "(A)"
#endif

#if defined(IS_ARRAY)
#if (TYPE_NUM == 0)
#define ARE_EQUAL(A, B) compare_all(A, B)
#else
#define ARE_EQUAL(A, B) all(RELATION(A, B))
#endif
#else
#define ARE_EQUAL(A, B) RELATION(A, B)
#endif

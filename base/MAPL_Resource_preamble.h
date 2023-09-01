#if defined(FMT_)
#undef FMT_
#endif

#if defined(TYPE_STRING_)
#undef TYPE_STRING_
#endif

#if defined(ARE_EQUAL)
#undef ARE_EQUAL
#endif

#if defined(REL_OP)
#undef REL_OP
#endif

#if (TYPE_NUM == 1) 
#define TYPE_STRING_ "'Integer*4 '"
#define FMT_ "(I0.1)"
#define REL_OP(A, B) (A == B)

#elif (TYPE_NUM == 2) 
#define TYPE_STRING_ "'Integer*8 '"
#define FMT_ "(I0.1)"
#define REL_OP(A, B) (A == B)

#elif (TYPE_NUM == 3) 
#define TYPE_STRING_ "'Real*4 '"
#define FMT_ "(F0.6)"
#define REL_OP(A, B) (A == B)

#elif (TYPE_NUM == 4) 
#define TYPE_STRING_ "'Real*8 '"
#define FMT_ "(F0.6)"
#define REL_OP(A, B) (A == B)

#elif (TYPE_NUM == 5) 
#define TYPE_STRING_ "'Logical '"
#define REL_OP(A, B) (A .eqv. B)
#define FMT_ "(L1)"

#elif (TYPE_NUM == 0) 
#define TYPE_STRING_ "'Character '"
#define REL_OP(A, B) (A == B)
#define FMT_ "(A)"
#endif
! TYPE_STRING_ , FMT_

#if defined(IS_ARRAY)
! array
#if (TYPE_NUM == 0)
! character
#define ARE_EQUAL(A, B) compare_all(A, B)
#else
! noncharacter
#define ARE_EQUAL(A, B) all(REL_OP(A, B))
#endif
#else
! scalar
#define ARE_EQUAL(A, B) REL_OP(A, B)
#endif
! ARE_EQUAL(A, B)

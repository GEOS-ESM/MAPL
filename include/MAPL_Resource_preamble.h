#ifdef MISMATCH_MESSAGE_
#   undef MISMATCH_MESSAGE_
#endif

#ifdef FMT_
#   undef FMT_
#endif

#ifdef TYPE_STRING_
#   undef TYPE_STRING_
#endif

#define MISMATCH_MESSAGE_ "Type of 'default' does not match type of 'value'."

#if (TYPE_ == "integer(int32)")
#   define TYPE_STRING_ "'Integer*4 '"
#   define FMT_ "(I0.1)"
#   define ARE_EQUAL(A, B) (A == B)
#elif (TYPE_ == "integer(int64)")
#   define TYPE_STRING_ "'Integer*8 '"
#   define FMT_ "(I0.1)"
#   define ARE_EQUAL(A, B) (A == B)
#elif (TYPE_ == "real(real32)")
#   define TYPE_STRING_ "'Real*4 '"
#   define FMT_ "(F0.6)"
#   define ARE_EQUAL(A, B) (A == B)
#elif (TYPE_ == "real(real64)")
#   define TYPE_STRING_ "'Real*8 '"
#   define FMT_ "(F0.6)"
#   define ARE_EQUAL(A, B) (A == B)
#elif (TYPE_ == "logical")
#   define TYPE_STRING_ "'Logical '"
#   define ARE_EQUAL(A, B) (A .eqv. B)
#   define FMT_ "(L1)"
#elif (TYPE_ == "character(len=*)")
#   define TYPE_STRING_ "'Character '"
#   define ARE_EQUAL(A, B) (A == B)
#   define FMT_ "(A)"
#endif

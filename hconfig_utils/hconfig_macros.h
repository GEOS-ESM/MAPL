! vim: ft=fortran
#define MAXSTRLEN ESMF_MAXSTR

#if !defined TFMT
#define TFMT '(G0)'
#endif

#if defined IS_STRING
#define WRITE_STATEMENT(C, S, V) C = '"' // trim(V) // '"'; S = 0
#undef VTYPE
#define VTYPE character(len=*)
#define MTYPE character(len=:)
#if defined IS_ARRAY
#define USE_STRLEN
#endif
#else
#define WRITE_STATEMENT(C, S, V) write(C, fmt=TFMT, iostat=S) V
#define MTYPE VTYPE
#endif

#if !defined RELOPR
#define RELOPR ==
#endif

#if defined IS_ARRAY
#define PROPFCT(A, B) all(A RELOPR B)
#define SZFCT size
#else
#define PROPFCT(A, B) A RELOPR B
#define SZFCT rank
#endif

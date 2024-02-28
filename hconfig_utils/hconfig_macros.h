#define MAXSTRLEN ESMF_MAXSTR

#if !defined TFMT
#define TFMT 'G0'
#endif

#if !defined MTYPE
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

#if !defined WRITE_STATEMENT
#define WRITE_STATEMENT(C, F, S, V) write(C, fmt=F, iostat=S) V
#endif

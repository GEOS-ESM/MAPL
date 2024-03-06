! vim: ft=fortran
#define MAXSTRLEN ESMF_MAXSTR

#if !defined TFMT
#  define TFMT '(G0)'
#endif

#if defined IS_STRING
#  define WRITE_STATEMENT(C, S, V) C = '"' // trim(V) // '"'; S = 0
#  define VTYPE character(len=*)
#  define MTYPE character(len=:), allocatable 
#else
#  define WRITE_STATEMENT(C, S, V) write(C, fmt=TFMT, iostat=S) V
#  define MTYPE VTYPE
#endif

#if !defined RELOPR
#  define RELOPR ==
#endif

#if defined IS_ARRAY
#  define PROPFCT(A, B) all(A RELOPR B)
#else
#  define SZFCT rank
#endif

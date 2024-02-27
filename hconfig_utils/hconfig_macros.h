#if !defined MTYPE
#define MTYPE VTYPE
#endif

#if !defined RELOPR
#define RELOPR ==
#endif

#if !defined WRITE_STATEMENT
#define WRITE_STATEMENT(RW, FT, ST, V) write(RW, fmt=FT, iostat=ST) V
#endif

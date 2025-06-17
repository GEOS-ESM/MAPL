#define TABLE   ESTBLE
#define TABLEX  ESTBLX

#if KIND_==4
#define FUNC Qsat0
#else
#define FUNC Qsatd0
#endif

#include "eqsatramp.H"

#undef TABLEX
#undef TABLE

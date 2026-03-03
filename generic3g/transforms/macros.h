#if defined(DP_)
#  define KIND_ ESMF_KIND_R8
#  define UNDEF_ MAPL_UNDEFINED_REAL64
#else
#  define KIND_ ESMF_KIND_R4
#  define UNDEF_ MAPL_UNDEFINED_REAL
#endif

#if defined(MAX_ACCUMULATOR_)
#  define MAXMIN_
#elif defined(MIN_ACCUMULATOR_)
#  define MAXMIN_
#endif
! vim: ft=fortran

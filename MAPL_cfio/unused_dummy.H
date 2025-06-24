! The following macro causes a variable to appear to be "used"
! according to the compiler.  This is a kludge to avoid excessive
! warnings.  In most cases, a better fix would be to modify the the
! procedure interface, but it is impractical in the short term.
!
! Note that the conditional is never satisfied and a reasonable
! compiler will optimize the line away.  (Hopefully without
! reintroducing the warning!)

#ifdef _unused_dummy
#  undef _unused_dummy
#endif
#define _unused_dummy(x) if (.false.) print*,shape(x)

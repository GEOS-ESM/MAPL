#define _SUCCESS 0

#if defined(_FAILURE)
#  undef _FAILURE
#endif
#define _FAILURE _SUCCESS-1

#define _VERIFY(status) \
   if(status /= 0) then; \
      call assert_that(status, is(0), location=SourceLocation(__FILE__,__LINE__)); \
      if (anyExceptions()) return; \
   endif
#define _RC rc=status); _VERIFY(status

#define _HERE print*,__FILE__,__LINE__

#if defined(_RETURN)
#  undef _RETURN
#endif
#define _RETURN(A) if(present(rc)) rc=A; return

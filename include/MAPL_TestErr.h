#define _SUCCESS 0
#define _VERIFY(status) \
   if(status /= 0) then; \
      call assert_that(status, is(0), location=SourceLocation(__FILE__,__LINE__)); \
      if (anyExceptions()) return; \
   endif
#define _RC rc=status); _VERIFY(status

#define _HERE print*,__FILE__,__LINE__

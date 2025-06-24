#define __RETURN(status) if(present(rc)) then; rc=status; return; endif
#define __RETURN_UNLESS(cond) if (.not. cond) then; __RETURN(UT_SUCCESS); endif
#define __ASSERT(cond, msg) if (.not. (cond)) then;  __RETURN(msg); endif
#define __RC rc=status); __ASSERT(rc==UT_SUCCESS, status

!rc=status); if (.not. (rc==UT_SUCCESS)) then; if(present(rc)) then; rc=status; return; endif; endif

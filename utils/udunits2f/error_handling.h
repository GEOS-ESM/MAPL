#define _RETURN(status) if(present(rc)) then; rc=status; return; endif
#define _RETURN_UNLESS(cond) if (.not. cond) then; _RETURN(UT_SUCCESS); endif
#define _ASSERT(cond, msg) if (.not. (cond)) then;  _RETURN(msg); endif
#define _RC rc=status); _ASSERT(rc==UT_SUCCESS, status

!rc=status); if (.not. (rc==UT_SUCCESS)) then; if(present(rc)) then; rc=status; return; endif; endif

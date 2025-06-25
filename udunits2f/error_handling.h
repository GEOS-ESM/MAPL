#define _return(status) if(present(rc)) then; rc=status; return; endif
#define _return_unless(cond) if (.not. cond) then; _return(UT_SUCCESS); endif
#define _assert(cond, msg) if (.not. (cond)) then;  _return(msg); endif
#define _rc rc=status); _assert(rc==UT_SUCCESS, status

!rc=status); if (.not. (rc==UT_SUCCESS)) then; if(present(rc)) then; rc=status; return; endif; endif

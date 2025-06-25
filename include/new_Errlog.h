
! The error logging may eventually evolve into a module based
! on the ESMF logger.  For now these macros provide simple
! traceback capability.

! new single underscore lowercase
#  ifdef _here
#     undef _here
#  endif
#  ifdef _return
#    undef _return
#  endif
#  ifdef _return_if
#    undef _return_if
#  endif
#  ifdef _return_unless
#    undef _return_unless
#  endif
#  ifdef _verify
#    undef _verify
#  endif
#  ifdef _assert
#    undef _assert
#  endif
#  ifdef _unused_dummy
#    undef _unused_dummy
#  endif
#  ifdef _file_
#    undef _file_
#  endif
#  ifdef _rc
#    undef _rc
#  endif
#  ifdef _userrc
#    undef _userrc
#  endif
#  ifdef _stat
#    undef _stat
#  endif
#  ifdef _iostat
#    undef _iostat
#  endif
#  ifdef _ierror
#    undef _ierror
#  endif
#  ifdef _return__
#    undef _return__
#  endif
#  ifdef _rc__
#    undef _rc__
#  endif


#  ifdef I_AM_MAIN
#    define _return__ call MAPL_abort()
#    define _rc__(rc)
#  else
#    define _return__ return
#    define _rc__(rc) ,rc
#  endif

#    define _here print*,__FILE__,__LINE__

#  ifndef ANSI_CPP

#    define _success 0
#    define _failure 1
#    define _unused_dummy(x) if (.false.) print*,shape(x)

#    define _file_ __FILE__
#    if defined(I_AM_FUNIT)
#       define _verify(A)     call assert_that(A, is(0), SourceLocation(_FILE_,__LINE__));if(anyExceptions())return
#    elif defined(I_AM_PFUNIT)
#       define _verify(A)     call assert_that(A, is(0), SourceLocation(_FILE_,__LINE__));if(anyExceptions(this%context))return
#    else
#       define _return(A)     call MAPL_Return(A,_FILE_,__LINE__ _rc__(rc)); _return__
#       define _return_if(cond)     if(cond)then;_return(_success);endif
#       define _return_unless(cond)     if(.not.(cond))then;_return(_success);endif
#       define _verify(A)     if(MAPL_Verify(A,_FILE_,__LINE__ _rc__(rc))) _return__
#    endif
#    define _rc_(rc,status) rc=status);_verify(status
#    define _userrc userRC=user_status, rc=status); _verify(status); _verify(user_status
#    define _rc _rc_(rc,status)

#    define _stat _rc_(stat,status)
#if defined(SUPPORT_FOR_MPI_IERROR_KEYWORD)
#    define _ierror _rc_(ierror,status)
#else
#    define _ierror _rc_(ierr,status)
#endif
#    define _iostat _rc_(iostat,status)

#    define _assert_msg_and_loc_and_rc(A,msg,stat,file,line,rc)  if(MAPL_Assert(A,msg,stat,file,line _rc__(rc))) _return__

! Assumes status is passed back in dummy called "rc"
#    define _assert_msg_and_loc(A,msg,stat,file,line) _assert_msg_and_loc_and_rc(A,msg,stat,file,line,rc)
! Assumes __FILE__ and __LINE__ are appropriate
#    define _assert(A,msg) _assert_msg_and_loc(A,msg,1,_FILE_,__LINE__)
#    define _assert_rc(A,msg,stat) _assert_msg_and_loc(A,msg,stat,_FILE_,__LINE__)
#    define _assert_nomsg(A) _assert(A,'needs informative message')
#    define _fail(msg) _assert(.false.,msg)

#  endif


! vim: set filetype=fortran:

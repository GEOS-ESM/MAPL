
! The error logging may eventually evolve into a module based
! on the ESMF logger.  For now these macros provide simple
! traceback capability.

#  ifdef RETURN_
#    undef RETURN_
#  endif
#  ifdef VERIFY_
#    undef VERIFY_
#  endif
#  ifdef ASSERT_
#    undef ASSERT_
#  endif
#  ifdef IGNORE_
#    undef IGNORE_
#  endif

! new
#  ifdef _HERE
#     undef _HERE
#  endif
#  ifdef _RETURN
#    undef _RETURN
#  endif
#  ifdef _RETURN_IF
#    undef _RETURN_IF
#  endif
#  ifdef _RETURN_UNLESS
#    undef _RETURN_UNLESS
#  endif
#  ifdef _VERIFY
#    undef _VERIFY
#  endif
#  ifdef _ASSERT
#    undef _ASSERT
#  endif
#  ifdef _UNUSED_DUMMY
#    undef _UNUSED_DUMMY
#  endif
#  ifdef _FILE_
#    undef _FILE_
#  endif
#  ifdef _RC
#    undef _RC
#  endif
#  ifdef _USERRC
#    undef _USERRC
#  endif
#  ifdef _STAT
#    undef _STAT
#  endif
#  ifdef _IOSTAT
#    undef _IOSTAT
#  endif
#  ifdef _IERROR
#    undef _IERROR
#  endif

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


#  define IGNORE_(a) continue

#  ifdef I_AM_MAIN
#    define _return__ call MAPL_abort()
#    define _rc__(rc)
#  else
#    define _return__ return
#    define _rc__(rc) ,rc
#  endif

#    define _HERE print*,__FILE__,__LINE__

#  ifdef ANSI_CPP

#    define RETURN_(...)   if(MAPL_RTRN(__VA_ARGS__,Iam,__LINE__ _rc__(rc))) _return__
#    define VERIFY_(...)   if(MAPL_VRFY(__VA_ARGS__,Iam,__LINE__ _rc__(rc))) _return__
#    define ASSERT_(...)   if(MAPL_ASRT(__VA_ARGS__,Iam,__LINE__ _rc__(rc))) _return__

#  else

! Old
#    define RETURN_(A)     if(MAPL_RTRN(A,Iam,__LINE__ _rc__(rc))) _return__
#    define VERIFY_(A)     if(MAPL_VRFY(A,Iam,__LINE__ _rc__(rc))) _return__
#    define ASSERT_(A)     if(MAPL_ASRT(A,Iam,__LINE__ _rc__(rc))) _return__

! New
#    define _success 0
#    define _SUCCESS _success
#    define _failure 1
#    define _FAILURE _failure
#    define _unused_dummy(x) if (.false.) print*,shape(x)
#    define _UNUSED_DUMMY(x) _unused_dummy(x)


#    define _file_ __FILE__
#    define _FILE_ _file_
#    if defined(I_AM_FUNIT)
#       define _verify(A)     call assert_that(A, is(0), SourceLocation(_FILE_,__LINE__));if(anyExceptions())return
#       define _VERIFY(A) _verify(a)
#    elif defined(I_AM_PFUNIT)
#       define _verify(A)     call assert_that(A, is(0), SourceLocation(_FILE_,__LINE__));if(anyExceptions(this%context))return
#       define _VERIFY(A) _verify(a)
#    else
#       define _return(A)     call MAPL_Return(A,_FILE_,__LINE__ _rc__(rc)); _return__
#       define _RETURN(A) _return(A)
#       define _return_if(cond)     if(cond)then;_return(_success);endif
#       define _RETURN_IF(cond) _return_if(cond)
#       define _return_unless(cond)     if(.not.(cond))then;_return(_success);endif
#       define _RETURN_UNLESS(cond) _return_unless(cond)
#       define _verify(A)     if(MAPL_Verify(A,_FILE_,__LINE__ _rc__(rc))) _return__
#       define _VERIFY(A) _verify(A)
#    endif
#    define _rc_(rc,status) rc=status);_verify(status
#    define _RC_(rc,status) _rc_(rc,status)
#    define _userrc userRC=user_status, rc=status); _verify(status); _verify(user_status
#    define _USERRC _userrc
#    define _rc _rc_(rc,status)
#    define _RC _rc_(rc,status)

#    define _stat _rc_(stat,status)
#    define _STAT _rc_(stat,status)
#if defined(SUPPORT_FOR_MPI_IERROR_KEYWORD)
#    define _ierror _rc_(ierror,status)
#    define _IERROR _rc_(ierror,status)
#else
#    define _ierror _rc_(ierr,status)
#    define _IERROR _rc_(ierr,status)
#endif
#    define _iostat _rc_(iostat,status)
#    define _IOSTAT _rc_(iostat,status)

#    define _assert_msg_and_loc_and_rc(A,msg,stat,file,line,rc)  if(MAPL_Assert(A,msg,stat,file,line _rc__(rc))) _return__

! Assumes status is passed back in dummy called "rc"
#    define _assert_msg_and_loc(A,msg,stat,file,line) _assert_msg_and_loc_and_rc(A,msg,stat,file,line,rc)
! Assumes __FILE__ and __LINE__ are appropriate
#    define _assert(A,msg) _assert_msg_and_loc(A,msg,1,_FILE_,__LINE__)
#    define _ASSERT(A,msg) _assert(A,msg)
#    define _assert_rc(A,msg,stat) _assert_msg_and_loc(A,msg,stat,_FILE_,__LINE__)
#    define _ASSERT_RC(A,msg,stat) _assert_rc(A,msg,stat)
#    define _assert_nomsg(A) _assert(A,'needs informative message')
#    define _ASSERT_NOMSG(A) _assert_nomsg(A)
#    define _fail(msg) _assert(.false.,msg)
#    define _FAIL(msg) _fail(msg)

#  endif




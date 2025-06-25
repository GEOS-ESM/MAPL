
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


#  define IGNORE_(a) continue

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
#    define _SUCCESS _success
#    define _FAILURE _failure
#    define _UNUSED_DUMMY(x) _unused_dummy(x)


#    define _FILE_ _file_
#    if defined(I_AM_FUNIT)
#       define _VERIFY(A) _verify(a)
#    elif defined(I_AM_PFUNIT)
#       define _VERIFY(A) _verify(a)
#    else
#       define _RETURN(A) _return(A)
#       define _RETURN_IF(cond) _return_if(cond)
#       define _RETURN_UNLESS(cond) _return_unless(cond)
#       define _VERIFY(A) _verify(A)
#    endif
#    define _RC_(rc,status) _rc_(rc,status)
#    define _USERRC _userrc
#    define _RC _rc_(rc,status)

#    define _STAT _rc_(stat,status)
#if defined(SUPPORT_FOR_MPI_IERROR_KEYWORD)
#    define _IERROR _rc_(ierror,status)
#else
#    define _IERROR _rc_(ierr,status)
#endif
#    define _IOSTAT _rc_(iostat,status)

! Assumes status is passed back in dummy called "rc"
! Assumes __FILE__ and __LINE__ are appropriate
#    define _ASSERT(A,msg) _assert(A,msg)
#    define _ASSERT_RC(A,msg,stat) _assert_rc(A,msg,stat)
#    define _ASSERT_NOMSG(A) _assert_nomsg(A)
#    define _FAIL(msg) _fail(msg)

#  endif


! vim: set filetype=fortran:

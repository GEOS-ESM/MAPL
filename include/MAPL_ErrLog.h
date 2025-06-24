
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
#  ifdef __return
#    undef __return
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
#  ifdef __return
#    undef __return
#  endif
#  ifdef __rc
#    undef __rc
#  endif

! new double-underscore
#  ifdef __HERE
#     undef __HERE
#  endif
#  ifdef __RETURN
#    undef __RETURN
#  endif
#  ifdef __RETURN_IF
#    undef __RETURN_IF
#  endif
#  ifdef __RETURN_UNLESS
#    undef __RETURN_UNLESS
#  endif
#  ifdef __VERIFY
#    undef __VERIFY
#  endif
#  ifdef __ASSERT
#    undef __ASSERT
#  endif
#  ifdef __UNUSED_DUMMY
#    undef __UNUSED_DUMMY
#  endif
#  ifdef __RC
#    undef __RC
#  endif
#  ifdef __USERRC
#    undef __USERRC
#  endif
#  ifdef __STAT
#    undef __STAT
#  endif
#  ifdef __IOSTAT
#    undef __IOSTAT
#  endif
#  ifdef __IERROR
#    undef __IERROR
#  endif
#  ifdef __return
#    undef __return
#  endif
#  ifdef __rc
#    undef __rc
#  endif

#  define IGNORE_(a) continue

#  ifdef I_AM_MAIN
#    define __return call MAPL_abort()
#    define __rc(rc)
#  else
#    define __return return
#    define __rc(rc) ,rc
#  endif

#    define __HERE print*,__FILE__,__LINE__
#    define _HERE __HERE__

#  ifdef ANSI_CPP

#    define RETURN_(...)   if(MAPL_RTRN(__VA_ARGS__,Iam,__LINE__ __rc(rc))) __return
#    define VERIFY_(...)   if(MAPL_VRFY(__VA_ARGS__,Iam,__LINE__ __rc(rc))) __return
#    define ASSERT_(...)   if(MAPL_ASRT(__VA_ARGS__,Iam,__LINE__ __rc(rc))) __return

#  else

! Old
#    define RETURN_(A)     if(MAPL_RTRN(A,Iam,__LINE__ __rc(rc))) __return
#    define VERIFY_(A)     if(MAPL_VRFY(A,Iam,__LINE__ __rc(rc))) __return
#    define ASSERT_(A)     if(MAPL_ASRT(A,Iam,__LINE__ __rc(rc))) __return

! New
#    define __SUCCESS 0
#    define _SUCCESS __SUCCESS
#    define __FAILURE 1
#    define _FAILURE __FAILURE
#    define __UNUSED_DUMMY(x) if (.false.) print*,shape(x)
#    define _UNUSED_DUMMY(x) __UNUSED_DUMMY(x)


#    define _FILE_ __FILE__
#    if defined(I_AM_FUNIT)
#       define __VERIFY(A) call assert_that(A, is(0), SourceLocation(_FILE_,__LINE__));if(anyExceptions())return
#       define _VERIFY(A) __VERIFY(A)
#    elif defined(I_AM_PFUNIT)
#       define __VERIFY(A) call assert_that(A, is(0), SourceLocation(_FILE_,__LINE__));if(anyExceptions(this%context))return
#       define _VERIFY(A) __VERIFY(A)
#    else
#       define __RETURN(A) call MAPL_Return(A,_FILE_,__LINE__ __rc(rc)); __return
#       define _RETURN(A) __RETURN(A)
#       define __RETURN_IF(cond) if(cond)then;_RETURN(_SUCCESS);endif
#       define _RETURN_IF(cond) __RETURN_IF(cond)
#       define __RETURN_UNLESS(cond) if(.not.(cond))then;_RETURN(_SUCCESS);endif
#       define _RETURN_UNLESS(cond) __RETURN_UNLESS(cond)
#       define __VERIFY(A)     if(MAPL_Verify(A,_FILE_,__LINE__ __rc(rc))) __return
#       define _VERIFY(A) __VERIFY(A)
#    endif
#    define __RC__(rc,status) rc=status);__VERIFY(status
#    define _RC_(rc,status) __RC__(rc,status)
#    define __USERRC userRC=user_status, rc=status); _VERIFY(status); _VERIFY(user_status
#    define _USERRC __USERRC
#    define __RC __RC__(rc,status)
#    define _RC __RC__(rc,status)

#    define __STAT __RC__(stat,status)
#    define _STAT __STAT
#if defined(SUPPORT_FOR_MPI_IERROR_KEYWORD)
#    define __IERROR __RC__(ierror,status)
#    define _IERROR __IERROR
#else
#    define __IERROR __RC__(ierr,status)
#    define _IERROR __IERROR
#endif
#    define __IOSTAT __RC__(iostat,status)
#    define _IOSTAT __IOSTAT

#    define __ASSERT_MSG_AND_LOC_AND_RC(A,msg,stat,file,line,rc)  if(MAPL_Assert(A,msg,stat,file,line __rc(rc))) __return
#    define _ASSERT_MSG_AND_LOC_AND_RC(A,msg,stat,file,line,rc) __ASSERT_MSG_AND_LOC_AND_RC(A,msg,stat,file,line,rc)

! Assumes status is passed back in dummy called "rc"
#    define __ASSERT_MSG_AND_LOC(A,msg,stat,file,line) _ASSERT_MSG_AND_LOC_AND_RC(A,msg,stat,file,line,rc)
#    define _ASSERT_MSG_AND_LOC(A,msg,stat,file,line) __ASSERT_MSG_AND_LOC(A,msg,stat,file,line)
! Assumes __FILE__ and __LINE__ are appropriate
#    define __ASSERT(A,msg) __ASSERT_MSG_AND_LOC(A,msg,1,_FILE_,__LINE__)
#    define _ASSERT(A,msg) __ASSERT(A,msg)
#    define __ASSERT_RC(A,msg,stat) __ASSERT_MSG_AND_LOC(A,msg,stat,_FILE_,__LINE__)
#    define _ASSERT_RC(A,msg,stat) __ASSERT_RC(A,msg,stat)
#    define __ASSERT_NOMSG(A) __ASSERT(A,'needs informative message')
#    define _ASSERT_NOMSG(A) __ASSERT_NOMSG(A)
#    define __FAIL(msg) __ASSERT(.false.,msg)
#    define _FAIL(msg) __FAIL(msg)

#  endif




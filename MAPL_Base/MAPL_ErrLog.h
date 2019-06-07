
!  $Id$ 

! The error logging may eventually evolve into a module based
! on the ESMF logger.  For now these macros provide simple
! traceback capability. 

#ifndef MAPL_ErrLog_DONE


#  define MAPL_ErrLog_DONE

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
#  ifdef _RETURN
#    undef _RETURN
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
#  ifdef _FILE
#    undef _FILE_
#  endif
#  ifdef _RC
#    undef _RC
#  endif
#  ifdef __return
#    undef __return
#  endif
#  ifdef __rc
#    undef __rc
#  endif

#  define IGNORE_(a) continue

#  ifdef I_AM_MAIN
#    define __return call MAPL_Abort()
#    define __rc(rc) 
#  else
#    define __return return
#    define __rc(rc) ,rc
#  endif

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
#    define _SUCCESS 0
#    define _FAILURE 1
#    define _UNUSED_DUMMY(x) if (.false.) print*,shape(x)


#    define _FILE_ __FILE__
#    define _RETURN(A)     call MAPL_Return(A,_FILE_,__LINE__ __rc(rc)); __return
#    define _VERIFY(A)     if(MAPL_Verify(A,_FILE_,__LINE__ __rc(rc))) __return
#    define _RC_(rc,status) rc=status);_VERIFY(status
#    define _RC _RC_(rc,status)


#    define _ASSERT_MSG_AND_LOC_AND_RC(A,msg,file,line,rc)  if(MAPL_Assert(A,msg,file,line __rc(rc))) __return

! Assumes status is passed back in dummy called "rc"
#    define _ASSERT_MSG_AND_LOC(A,msg,file,line) _ASSERT_MSG_AND_LOC_AND_RC(A,msg,file,line,rc)
! Assumes __FILE__ and __LINE__ are appropriate
#    define _ASSERT(A,msg) _ASSERT_MSG_AND_LOC(A,msg,_FILE_,__LINE__)
#    define _ASSERT_NOMSG(A) _ASSERT(A,'needs informative message')
#    define _FAIL(msg) _ASSERT(.false.,msg)

#  endif


#endif




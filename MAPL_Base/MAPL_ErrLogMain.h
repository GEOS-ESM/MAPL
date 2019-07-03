
!  $Id$ 

! The error logging may eventually evolve into a module based
! on the ESMF logger.  For now these macros provide simple
! traceback capability. 

#ifndef MAPL_ErrLogMain_DONE
#define MAPL_ErrLogMain_DONE

#ifdef _VERIFY
#undef _VERIFY
#endif

#ifdef _VERIFY
#undef _VERIFY
#endif

#ifdef ASSERT_
#undef ASSERT_
#endif

#define _VERIFY(A) if(MAPL_VRFY(A,Iam,__LINE__,RC))call MAPL_Abort

#define _VERIFY(A) if(MAPL_VRFY(A,Iam,__LINE__,RC))call MAPL_Abort

#define _ASSERT(A) if(MAPL_ASRT(A,Iam,__LINE__,RC),'needs informative message')call MAPL_Abort

#endif

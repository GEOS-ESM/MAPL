#ifdef VERIFY_ESMF_
#undef VERIFY_ESMF_
#endif

#ifdef VERIFY_ALL_ESMF_
#undef VERIFY_ALL_ESMF_
#endif

#ifdef VERIFY_NUOPC_
#undef VERIFY_NUOPC_
#endif

#define VERIFY_ESMF_(A) if(ESMF_LogFoundError(A, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#define VERIFY_ALL_ESMF_(A, B) VERIFY_ESMF_(A); VERIFY_ESMF_(B)
#define VERIFY_NUOPC_(A) if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

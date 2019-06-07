subroutine MAPL_Initialize()
   use ESMF
   use MAPL_ThrowMod
   use MAPL_pFUnit_ThrowMod
   
   call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE)
   call MAPL_set_throw_method(throw)

end subroutine MAPL_Initialize

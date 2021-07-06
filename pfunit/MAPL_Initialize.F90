module MAPL_pFUnit_Initialize
contains
   subroutine Initialize()
      use ESMF
      use MAPL_ThrowMod, only: MAPL_set_throw_method
      use MAPL_pFUnit_ThrowMod
   
      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_MULTI)
      call MAPL_set_throw_method(throw)

   end subroutine Initialize
end module MAPL_pFUnit_Initialize

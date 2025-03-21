module MAPL_pFUnit_Initialize
contains
   subroutine Initialize()
      use ESMF
      use MAPL_ThrowMod, only: MAPL_set_throw_method
      use MAPL_pFUnit_ThrowMod
      use pflogger, only: pfl_initialize => initialize
      use udunits2f, only: UDUNITS_Initialize => Initialize

      call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_MULTI,defaultCalKind=ESMF_CALKIND_GREGORIAN)
      call MAPL_set_throw_method(throw)
      call pfl_initialize()
      call UDUNITS_Initialize()

   end subroutine Initialize
end module MAPL_pFUnit_Initialize

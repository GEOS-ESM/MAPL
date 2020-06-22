module pFIO_pFUNIT_Initialize

contains
  subroutine Initialize()
     use MAPL_pFUnit_ThrowMod
     use MAPL_ExceptionHandling
   
     call MAPL_set_throw_method(throw)

   end subroutine Initialize
end module

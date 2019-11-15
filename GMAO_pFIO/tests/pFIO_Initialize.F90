module pFIO_pFUNIT_Initialize

contains
  subroutine Initialize()
     use ESMF
     use MAPL_pFUnit_ThrowMod
     use pFIO_ThrowMod
   
     call pFIO_set_throw_method(throw)

   end subroutine Initialize
end module

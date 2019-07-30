subroutine pFIO_Initialize()
   use ESMF
   use MAPL_pFUnit_ThrowMod
   use pFIO_ThrowMod
   
   call pFIO_set_throw_method(throw)

end subroutine PFIO_Initialize

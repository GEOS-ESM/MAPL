module pFIO_AbstractRequestHandleMod
   use pFIO_AbstractDataReferenceMod
   implicit none
   private

   public :: AbstractRequestHandle

   type, abstract :: AbstractRequestHandle
      class (AbstractDataReference),allocatable :: data_reference
   contains
      procedure(wait), deferred :: wait
   end type AbstractRequestHandle

   abstract interface
      subroutine wait(this)
         import AbstractRequestHandle
         class (AbstractRequestHandle), intent(inout) :: this
      end subroutine wait
   end interface


end module pFIO_AbstractRequestHandleMod

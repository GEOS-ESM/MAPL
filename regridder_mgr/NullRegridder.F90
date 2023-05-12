#include "MAPL_Generic.h"

module mapl_NullRegridder
   use esmf
   use mapl_Regridder
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: NULL_REGRIDDER

   type, extends(Regridder) :: NullRegridder
      private
   contains
      procedure :: regrid_scalar
   end type NullRegridder

   type(NullRegridder), parameter :: NULL_REGRIDDER = NullRegridder()

contains

   function new_NullRegridder() result(regriddr)
      type(NullRegridder) :: regriddr
   end function new_NullRegridder

   subroutine regrid_scalar(this, fv_in, fv_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: fv_in, fv_out
      integer, optional, intent(out) :: rc

      _FAIL('Null regridder')
   end subroutine regrid_scalar

end module mapl_Regridder
      

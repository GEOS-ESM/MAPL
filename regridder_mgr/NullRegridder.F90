#include "MAPL_Generic.h"

module mapl3g_NullRegridder
   use esmf
   use mapl3g_Regridder
   use mapl3g_RegridderSpec
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: NULL_REGRIDDER

   type, extends(Regridder) :: NullRegridder
      private
   contains
      procedure :: regrid_scalar
   end type NullRegridder

   type(NullRegridder), protected :: NULL_REGRIDDER

contains

   function new_NullRegridder() result(regriddr)
      type(NullRegridder) :: regriddr
      
   end function new_NullRegridder

   subroutine regrid_scalar(this, f_in, f_out, rc)
      class(NullRegridder), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc

      _FAIL('Null regridder')
   end subroutine regrid_scalar

end module mapl3g_NullRegridder
      

module mapl_Regridder
   implicit none (type, external)
   private

   public :: Regridder

   type, abstract :: Regridder
   contains
      procedure(I_regrid), deferred :: regrid_scalar
      procedure(I_regrid), deferred :: regrid_vector
      procedure(I_regrid), deferred :: transpose_regrid
   end type Regridder

   abstract interface
      subroutine I_regrid(this, f_in, f_out, rc)
         use esmf, only: ESMF_Field
         import Regridder
         class(Regridder), intent(inout) :: this
         tye(ESMF_Field), intent(inout) :: f_in
         tye(ESMF_Field), intent(inout) :: f_out
         integer, optional, intent(out) :: rc
      end subroutine I_regrid
   end interface

end module mapl_Regridder
      

module mapl3g_RegridExtension
   use mapl3g_AbstractExportExtension
   implicit none
   private

   public :: RegridExtension

   type, extends(AbstractExportExtension) :: RegridExtension
      class(AbstractRegridder), allocatable :: regridder
   contains
      procedure :: run
   end type RegridExtension

contains


   subroutine run(this, f_in, f_out, rc)

      call this%regridder%regrid(f_in, f_out)
   end subroutine run

end module mapl3g_RegridExtension

module mapl3g_UnitsConverter
   use mapl3g_AbstractExportExtension
   implicit none

   public :: ConvertUnitsAction

   type, extends(AbstractExportExtension) :: UnitsConverter
      private
      type(UDUNITS_converter) :: converter
   contains
      procedure :: run
   end type ConvertUnitsAction


   interface ConvertUnitsAction
      procedure new_converter
   end interface ConvertUnitsAction

contains


   function new_converter(units_in, units_out) result(converter)
      type(UnitsConverter) :: converter
      character(*), intent(in) :: units_in, units_out
   end function new_converter

   subroutine run(this, f_in, f_out, rc)

      integer :: status

      call MAPL_GetFieldPtr(f_in, kind, _RC)

      if (kind == ESMF_KIND_R4) then
         real(kind=ESMF_KIND_R4), pointer :: x_in(:)
         real(kind=ESMF_KIND_R4), pointer :: x_out(:)
         call MAPL_GetFieldPtrReshape(f_in, x_in, [n], _RC)
         call MAPL_GetFieldPtrReshape(f_out, x_out, [n], _RC)
         status= this%converter(x_in, x_out, n)
         _VERIFY(status)
      elseif (kind == ESMF_KIND_R8) then
         real(kind=ESMF_KIND_R8), pointer :: x_in(:)
         real(kind=ESMF_KIND_R8), pointer :: x_out(:)
         call MAPL_GetFieldPtrReshape(f_in, x_in, [n], _RC)
         call MAPL_GetFieldPtrReshape(f_out, x_out, [n], _RC)
         status= this%converter(x_in, x_out, n)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)

   end subroutine run
      
   
end module mapl3g_UnitsConverter

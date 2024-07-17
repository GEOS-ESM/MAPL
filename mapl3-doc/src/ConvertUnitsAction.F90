#include "MAPL_Generic.h"

module mapl3g_ConvertUnitsAction
   use mapl3g_ExtensionAction
   use udunits2f, only: UDUNITS_Converter => Converter
   use udunits2f, only: UDUNITS_GetConverter => get_converter
   use udunits2f, only: UDUNITS_Initialize => Initialize
   use MAPL_FieldUtils
   use mapl_ErrorHandling
   use esmf
   implicit none

   public :: ConvertUnitsAction

   type, extends(ExtensionAction) :: ConvertUnitsAction
      private
      type(UDUNITS_converter) :: converter
      type(ESMF_Field) :: f_in, f_out
   contains
      procedure :: run
   end type ConvertUnitsAction


   interface ConvertUnitsAction
      procedure new_converter
   end interface ConvertUnitsAction


contains


   function new_converter(f_in, units_in, f_out, units_out) result(action)
      type(ConvertUnitsAction) :: action
      type(ESMF_Field), intent(in) :: f_in, f_out
      character(*), intent(in) :: units_in, units_out

      integer :: status

      ! TODO: move to place where only called
      call UDUNITS_GetConverter(action%converter, from=units_in, to=units_out, rc=status)
      action%f_in = f_in
      action%f_out = f_out
      
   end function new_converter

   subroutine run(this, rc)
      class(ConvertUnitsAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind
      real(kind=ESMF_KIND_R4), pointer :: x4_in(:)
      real(kind=ESMF_KIND_R4), pointer :: x4_out(:)
      real(kind=ESMF_KIND_R8), pointer :: x8_in(:)
      real(kind=ESMF_KIND_R8), pointer :: x8_out(:)


      call ESMF_FieldGet(this%f_in, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then

         call assign_fptr(this%f_in, x4_in, _RC)
         call assign_fptr(this%f_out, x4_out, _RC)

         x4_out = this%converter%convert(x4_in)

      elseif (typekind == ESMF_TYPEKIND_R8) then

         call assign_fptr(this%f_in, x8_in, _RC)
         call assign_fptr(this%f_out, x8_out, _RC)

         x8_out = this%converter%convert(x8_in)
      end if

      _RETURN(_SUCCESS)
   end subroutine run
      
   
end module mapl3g_ConvertUnitsAction

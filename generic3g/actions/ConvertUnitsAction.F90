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
      character(:), allocatable :: src_units, dst_units
   contains
      procedure :: initialize
      procedure :: run_old
      procedure :: run_new
   end type ConvertUnitsAction


   interface ConvertUnitsAction
      procedure new_converter
      procedure new_converter2
   end interface ConvertUnitsAction


contains


   function new_converter(f_in, src_units, f_out, dst_units) result(action)
      type(ConvertUnitsAction) :: action
      type(ESMF_Field), intent(in) :: f_in, f_out
      character(*), intent(in) :: src_units, dst_units

      integer :: status

      ! TODO: move to place where only called
      call UDUNITS_GetConverter(action%converter, from=src_units, to=dst_units, rc=status)
      action%f_in = f_in
      action%f_out = f_out
      
   end function new_converter

   function new_converter2(src_units, dst_units) result(action)
      type(ConvertUnitsAction) :: action
      character(*), intent(in) :: src_units, dst_units

      action%src_units = src_units
      action%dst_units = dst_units

   end function new_converter2

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(ConvertUnitsAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status

      call UDUNITS_GetConverter(this%converter, from=this%src_units, to=this%dst_units, _RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine run_old(this, rc)
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
   end subroutine run_old
      
   subroutine run_new(this, importState, exportState, clock, rc)
      use esmf
      class(ConvertUnitsAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_Field) :: f_in, f_out
      real(kind=ESMF_KIND_R4), pointer :: x4_in(:)
      real(kind=ESMF_KIND_R4), pointer :: x4_out(:)
      real(kind=ESMF_KIND_R8), pointer :: x8_in(:)
      real(kind=ESMF_KIND_R8), pointer :: x8_out(:)

      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)

      call ESMF_FieldGet(f_in, typekind=typekind, _RC)
      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr(f_in, x4_in, _RC)
         call assign_fptr(f_out, x4_out, _RC)
         x4_out = this%converter%convert(x4_in)
         _RETURN(_SUCCESS)
      end if

      if (typekind == ESMF_TYPEKIND_R8) then
         call assign_fptr(f_in, x8_in, _RC)
         call assign_fptr(f_out, x8_out, _RC)
         x8_out = this%converter%convert(x8_in)
         _RETURN(_SUCCESS)
      end if

      _FAIL('unsupported typekind')

   end subroutine run_new
   
end module mapl3g_ConvertUnitsAction

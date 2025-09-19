#include "MAPL.h"

module mapl3g_ConvertUnitsTransform
   use mapl3g_TransformId
   use mapl3g_ExtensionTransform
   use udunits2f, only: UDUNITS_Converter => Converter
   use udunits2f, only: UDUNITS_GetConverter => get_converter
   use udunits2f, only: UDUNITS_Initialize => Initialize
   use MAPL_FieldUtils
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: ConvertUnitsTransform
   public :: IMPORT_FIELD_NAME
   public :: EXPORT_FIELD_NAME

   type, extends(ExtensionTransform) :: ConvertUnitsTransform
      private
      type(UDUNITS_converter) :: converter
      type(ESMF_Field) :: f_in, f_out
      character(:), allocatable :: src_units, dst_units
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
   end type ConvertUnitsTransform


   interface ConvertUnitsTransform
      procedure new_converter
   end interface ConvertUnitsTransform

   character(len=*), parameter :: IMPORT_FIELD_NAME = 'import[1]'
   character(len=*), parameter :: EXPORT_FIELD_NAME = 'export[1]'

contains


   function new_converter(src_units, dst_units) result(transform)
      type(ConvertUnitsTransform) :: transform
      character(*), intent(in) :: src_units, dst_units

      transform%src_units = src_units
      transform%dst_units = dst_units

   end function new_converter

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(ConvertUnitsTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status

      call UDUNITS_GetConverter(this%converter, from=this%src_units, to=this%dst_units, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

      
   subroutine update(this, importState, exportState, clock, rc)
      use esmf
      class(ConvertUnitsTransform), intent(inout) :: this
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

      call ESMF_StateGet(importState, itemName=IMPORT_FIELD_NAME, field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName=EXPORT_FIELD_NAME, field=f_out, _RC)

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
      _UNUSED_DUMMY(clock)
   end subroutine update
   
   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(ConvertUnitsTransform), intent(in) :: this

      id = UNITS_TRANSFORM_ID
   end function get_transformId

end module mapl3g_ConvertUnitsTransform

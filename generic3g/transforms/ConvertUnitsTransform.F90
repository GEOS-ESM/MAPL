#include "MAPL.h"
#include "unused_dummy.H"

module mapl3g_ConvertUnitsTransform
   use mapl3g_TransformId
   use mapl3g_StateItem
   use mapl3g_ExtensionTransform
   use mapl3g_ExtensionTransformUtils, only: bundle_types_valid
   use udunits2f, only: UDUNITS_Converter => Converter
   use udunits2f, only: UDUNITS_GetConverter => get_converter
   use udunits2f, only: UDUNITS_Initialize => Initialize
   use MAPL_FieldUtils
!   use mapl3g_FieldBundle_API
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: ConvertUnitsTransform

   type, extends(ExtensionTransform) :: ConvertUnitsTransform
      private
      type(UDUNITS_converter) :: converter
      character(:), allocatable :: src_units, dst_units
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
   end type ConvertUnitsTransform


   interface ConvertUnitsTransform
      procedure new_converter
   end interface ConvertUnitsTransform

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

   subroutine update_field(f_in, f_out, converter, rc)
      type(ESMF_Field), intent(inout) :: f_in, f_out
      type(UDUNITS_converter), intent(in) :: converter
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: x4_in(:)
      real(kind=ESMF_KIND_R4), pointer :: x4_out(:)
      real(kind=ESMF_KIND_R8), pointer :: x8_in(:)
      real(kind=ESMF_KIND_R8), pointer :: x8_out(:)
      type(ESMF_TypeKind_Flag) :: typekind

      call ESMF_FieldGet(f_in, typekind=typekind, _RC)
      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr(f_in, x4_in, _RC)
         call assign_fptr(f_out, x4_out, _RC)
         x4_out = converter%convert(x4_in)
         _RETURN(_SUCCESS)
      end if

      if (typekind == ESMF_TYPEKIND_R8) then
         call assign_fptr(f_in, x8_in, _RC)
         call assign_fptr(f_out, x8_out, _RC)
         x8_out = converter%convert(x8_in)
         _RETURN(_SUCCESS)
      end if

      _FAIL('unsupported typekind')

   end subroutine update_field
      
   subroutine update_field_bundle(fb_in, fb_out, converter, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      type(UDUNITS_Converter), intent(in) :: converter
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i, fieldCount
      type(ESMF_Field), allocatable :: flist_in(:), flist_out(:)

      call ESMF_FieldBundleGet(fb_out, fieldCount=fieldCount, _RC)
      call ESMF_FieldBundleGet(fb_in, fieldCount=i, _RC)
      _ASSERT(i==fieldCount, 'The number of ESMF_Field''s in the ESMF_Bundles'' do not match.')
      allocate(flist_in(fieldCount))
      allocate(flist_out(fieldCount))
      call ESMF_FieldBundleGet(fb_in, fieldList=flist_in, _RC)
      call ESMF_FieldBundleGet(fb_out, fieldList=flist_out, _RC)
      _ASSERT(size(flist_in) == size(flist_out), 'The FieldBundles have different sizes.')
      do i=1, size(flist_in)
         call update_field(flist_in(i), flist_out(i), converter, _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine update_field_bundle

   subroutine update(this, importState, exportState, clock, rc)
      use esmf
      class(ConvertUnitsTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out
      type(ESMF_FieldBundle) :: fb_in, fb_out
      type(ESMF_StateItem_Flag) :: itemtype_in, itemtype_out

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, itemtype=itemtype_in, _RC)
      call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, itemtype=itemtype_out, _RC)
      _ASSERT(itemtype_in == itemtype_out, "Mismatched item types.")

      if(itemtype_in == MAPL_STATEITEM_FIELD) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, field=f_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=f_out, _RC)
         call update_field(f_in, f_out, this%converter, _RC)
      elseif(itemType_in == MAPL_STATEITEM_FIELDBUNDLE) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, fieldBundle=fb_out, _RC)
         call bundle_types_valid(fb_in, fb_out, _RC)
         call update_field_bundle(fb_in, fb_out, this%converter, _RC)
      else
         _FAIL("Unsupported state item type")
      end if

      _UNUSED_DUMMY(clock)

   end subroutine update

!   subroutine bundle_types_valid(b1, b2, rc)
!      type(ESMF_FieldBundle), intent(inout) :: b1, b2
!      integer, intent(out) :: rc
!      integer :: status
!      type(FieldBundleType_Flag) :: bt1, bt2
!      type(FieldBundleType_Flag), parameter :: ALLOWED_BUNDLE_TYPES(*) = [&
!         & FIELDBUNDLETYPE_BASIC, &
!         & FIELDBUNDLETYPE_BRACKET, &
!         & FIELDBUNDLETYPE_VECTOR, &
!         & FIELDBUNDLETYPE_VECTOR_BRACKET&
!         &]
!      character(len=:), allocatable :: msg

!      call MAPL_FieldBundleGet(b1, fieldBundleType=bt1, _RC)
!      msg = bt1%to_string()
!      _ASSERT(any(ALLOWED_BUNDLE_TYPES == bt1), 'FieldBundleType ' // msg // ' is not supported.')
!      call MAPL_FieldBundleGet(b2, fieldBundleType=bt2, _RC)
!      msg = '(' // msg // ', ' // bt2%to_string() // ')'
!      _ASSERT(bt1 == bt2, 'FieldBundleType values ' // msg // ' do not match.')

!   end subroutine bundle_types_valid
   
   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(ConvertUnitsTransform), intent(in) :: this

      id = UNITS_TRANSFORM_ID
      _UNUSED_DUMMY(this)

   end function get_transformId

end module mapl3g_ConvertUnitsTransform

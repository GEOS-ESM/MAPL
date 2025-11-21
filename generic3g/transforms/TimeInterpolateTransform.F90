#include "MAPL.h"

module mapl3g_TimeInterpolateTransform
   use mapl3g_TransformId
   use mapl3g_ExtensionTransform
   use mapl3g_regridder_mgr
   use mapl3g_FieldBundle_API
   use mapl3g_InfoUtilities
   use mapl3g_StateItem
   use MAPL_FieldUtils
   use MAPL_Constants, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use mapl_ErrorHandling
   use esmf

   implicit none(type,external)
   private

   public :: TimeInterpolateTransform

   type, extends(ExtensionTransform) :: TimeInterpolateTransform
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
   end type TimeInterpolateTransform

   interface TimeInterpolateTransform
      module procedure :: new_TimeInterpolateTransform
   end interface TimeInterpolateTransform

contains

   function new_TimeInterpolateTransform() result(transform)
      type(TimeInterpolateTransform) :: transform
   end function new_TimeInterpolateTransform

   subroutine initialize(this, importState, exportState, clock, rc)
      class(TimeInterpolateTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      ! noop

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      class(TimeInterpolateTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: itemType
      type(ESMF_FieldBundle) :: bundle_in
      type(ESMF_Field) :: field_out
      type(ESMF_FieldBundle) :: bundle_out
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_Field), allocatable :: field_list(:)

      call ESMF_StateGet(importState, COUPLER_IMPORT_NAME, itemType=itemType, _RC)
      _ASSERT(itemType == ESMF_STATEITEM_FIELDBUNDLE, 'Expected FieldBundle in importState.')

      call ESMF_StateGet(exportState, COUPLER_EXPORT_NAME, itemType=itemType, _RC)
      _ASSERT(itemType == ESMF_STATEITEM_FIELD .or. itemType == MAPL_STATEITEM_FIELDBUNDLE, 'Expected Field or Vector in exportState.')

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldbundle=bundle_in, _RC)
      if (itemType == ESMF_STATEITEM_FIELD) then
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=field_out, _RC)
         call ESMF_FieldGet(field_out, typekind=typekind, _RC)
      else if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME,fieldbundle=bundle_out, _RC)
         call MAPL_FieldBundleGet(bundle_out, fieldList=field_list, _RC)
         call ESMF_FieldGet(field_list(1), typekind=typekind, _RC)
      end if

      if (itemType ==ESMF_STATEITEM_FIELD) then
         if (typekind == ESMF_TYPEKIND_R4) then
            call run_r4(bundle_in, field_out, _RC)
            _RETURN(_SUCCESS)
         end if

         if (typekind == ESMF_TYPEKIND_R8) then
            call run_r8(bundle_in, field_out, _RC)
            _RETURN(_SUCCESS)
         end if
      else if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
         if (typekind == ESMF_TYPEKIND_R4) then
            call run_r4_vector(bundle_in, bundle_out, _RC)
            _RETURN(_SUCCESS)
         end if

         if (typekind == ESMF_TYPEKIND_R8) then
            call run_r8_vector(bundle_in, bundle_out, _RC)
            _RETURN(_SUCCESS)
         end if
      end if
      
      _FAIL('unexpected typekind')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine update

   subroutine run_r4(bundle_in, field_out, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      type(ESMF_Field), intent(inout) :: field_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: y(:), xi(:)
      real(kind=ESMF_KIND_R4), allocatable :: weights(:)
      integer :: i
      type(ESMF_Field), allocatable :: fieldList(:)
      type(ESMF_Info) :: bundle_info


      call MAPL_FieldBundleGet(bundle_in, fieldList=fieldList, interpolation_weights=weights, _RC)

      call assign_fptr(field_out, y, _RC)
      y = weights(1)
      do i = 1, size(fieldList)
         call assign_fptr(fieldList(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL .and. y /= MAPL_UNDEFINED_REAL)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      _RETURN(_SUCCESS)

   end subroutine run_r4

   subroutine run_r8(bundle_in, field_out, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      type(ESMF_Field), intent(inout) :: field_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: y(:), xi(:)
      real(kind=ESMF_KIND_R4), allocatable :: weights(:)
      integer :: i
      type(ESMF_Field), allocatable :: fieldList(:)
      type(ESMF_Info) :: bundle_info


      call MAPL_FieldBundleGet(bundle_in, fieldList=fieldList, interpolation_weights=weights, _RC)

      call assign_fptr(field_out, y, _RC)
      y = weights(1)
      do i = 1, size(fieldList)
         call assign_fptr(fieldList(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL64 .and. y /= MAPL_UNDEFINED_REAL64)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      _RETURN(_SUCCESS)

   end subroutine run_r8

   subroutine run_r4_vector(bundle_in, bundle_out, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      type(ESMF_FieldBundle), intent(inout) :: bundle_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: y(:), xi(:)
      real(kind=ESMF_KIND_R4), allocatable :: weights(:)
      integer :: i
      type(ESMF_Field), allocatable :: fieldList_in(:)
      type(ESMF_Field), allocatable :: fieldList_out(:)
      type(ESMF_Info) :: bundle_info


      call MAPL_FieldBundleGet(bundle_in, fieldList=fieldList_in, interpolation_weights=weights, _RC)
      call MAPL_FieldBundleGet(bundle_out, fieldList=fieldList_out, _RC)

      call assign_fptr(fieldList_out(1), y, _RC) ! u component
      y = weights(1)
      do i = 1, 3, 2
         call assign_fptr(fieldList_in(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL .and. y /= MAPL_UNDEFINED_REAL)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      call assign_fptr(fieldList_out(2), y, _RC) ! v component
      y = weights(1)
      do i = 2, 4, 2
         call assign_fptr(fieldList_in(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL .and. y /= MAPL_UNDEFINED_REAL)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      _RETURN(_SUCCESS)

   end subroutine run_r4_vector

   subroutine run_r8_vector(bundle_in, bundle_out, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      type(ESMF_FieldBundle), intent(inout) :: bundle_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: y(:), xi(:)
      real(kind=ESMF_KIND_R4), allocatable :: weights(:)
      integer :: i
      type(ESMF_Field), allocatable :: fieldList_in(:)
      type(ESMF_Field), allocatable :: fieldList_out(:)
      type(ESMF_Info) :: bundle_info

      call MAPL_FieldBundleGet(bundle_in, fieldList=fieldList_in, interpolation_weights=weights, _RC)
      call MAPL_FieldBundleGet(bundle_out, fieldList=fieldList_out, _RC)

      call assign_fptr(fieldList_out(1), y, _RC) ! u component
      y = weights(1)
      do i = 1, 3, 2
         call assign_fptr(fieldList_in(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL .and. y /= MAPL_UNDEFINED_REAL)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      call assign_fptr(fieldList_out(2), y, _RC) ! v component
      y = weights(1)
      do i = 2, 4, 2
         call assign_fptr(fieldList_in(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL .and. y /= MAPL_UNDEFINED_REAL)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      _RETURN(_SUCCESS)

   end subroutine run_r8_vector

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(TimeInterpolateTransform), intent(in) :: this

      id = TIME_INTERP_TRANSFORM_ID
   end function get_transformId

end module mapl3g_TimeInterpolateTransform

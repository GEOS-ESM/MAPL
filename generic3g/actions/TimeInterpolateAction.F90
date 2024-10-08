#include "MAPL_Generic.h"

module mapl3g_TimeInterpolateAction
   use mapl3g_ExtensionAction
   use mapl3g_regridder_mgr
   use mapl3g_InfoUtilities
   use MAPL_FieldUtils
   use MAPL_Constants, only: MAPL_UNDEFINED_REAL
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: TimeInterpolateAction

   type, extends(ExtensionAction) :: TimeInterpolateAction
   contains
      procedure :: initialize
      procedure :: run
   end type TimeInterpolateAction

   interface TimeInterpolateAction
      module procedure :: new_TimeInterpolateAction
   end interface TimeInterpolateAction

contains

   function new_TimeInterpolateAction() result(action)
      type(TimeInterpolateAction) :: action
   end function new_TimeInterpolateAction

   subroutine initialize(this, importState, exportState, clock, rc)
      class(TimeInterpolateAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      ! noop

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine run(this, importState, exportState, clock, rc)
      class(TimeInterpolateAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: itemType
      type(ESMF_FieldBundle) :: bundle_in
      type(ESMF_Field) :: field_out
      type(ESMF_TypeKind_Flag) :: typekind

      call ESMF_StateGet(importState, 'import[1]', itemType=itemType, _RC)
      _ASSERT(itemType == ESMF_STATEITEM_FIELDBUNDLE, 'Expected FieldBundle in importState.')

      call ESMF_StateGet(exportState, 'export[1]', itemType=itemType, _RC)
      _ASSERT(itemType == ESMF_STATEITEM_FIELD, 'Expected Field in exportState.')

      call ESMF_StateGet(importState, itemName='import[1]', fieldbundle=bundle_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=field_out, _RC)
      call ESMF_FieldGet(field_out, typekind=typekind, _RC)


      if (typekind == ESMF_TYPEKIND_R4) then
         call run_r4(bundle_in, field_out, _RC)
         _RETURN(_SUCCESS)
      end if

!#      if (typekind == ESMF_TYPEKIND_R8) then
!#         call run_r8(bundle_in, field_out, _RC)
!#         _RETURN(_SUCCESS)
!#      end if
      
      _FAIL('unexpected typekind')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine run


   subroutine run_r4(bundle_in, field_out, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      type(ESMF_Field), intent(inout) :: field_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: y(:), xi(:)
      real(kind=ESMF_KIND_R4), allocatable :: weights(:)
      integer :: i
      integer :: fieldCount
      type(ESMF_Field), allocatable :: fieldList(:)
      type(ESMF_Info) :: bundle_info


      call ESMF_FieldBundleGet(bundle_in, fieldCount=fieldCount, _RC)
      allocate(fieldList(fieldCount))
      call ESMF_FieldBundleGet(bundle_in, fieldList=fieldList, _RC)

      call MAPL_InfoGetInternal(bundle_in, 'weights', weights, _RC)

      call assign_fptr(field_out, y, _RC)
      y = weights(1)
      do i = 1, fieldCount
         call assign_fptr(fieldList(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL .and. y /= MAPL_UNDEFINED_REAL)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      _RETURN(_SUCCESS)

   end subroutine run_r4

end module mapl3g_TimeInterpolateAction

#include "MAPL.h"
#include "unused_dummy.H"

! A copy might be between different kinds and precisions, so is really
! a converter.  But ... what is a better name.
module mapl3g_CopyTransform
   use mapl3g_TransformId
   use mapl3g_ExtensionTransform
   use mapl3g_ExtensionTransformUtils
   use mapl3g_StateItem
   use mapl_ErrorHandling
   use esmf
   use MAPL_FieldUtils
   use mapl3g_FieldBundleCopy, only: FieldBundleCopy
   implicit none

   private
   public :: CopyTransform

   type, extends(ExtensionTransform) :: CopyTransform
      private
      type(ESMF_TypeKind_Flag) :: src_typekind
      type(ESMF_TypeKind_Flag) :: dst_typekind
      type(ESMF_Field) :: f_in, f_out
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
   end type CopyTransform

   interface CopyTransform
       module procedure new_CopyTransform
   end interface CopyTransform

contains

   ! We don't really need to know the typekind as the low level conversion routines
   ! will accept whatever is handed. So these arguments are more to preserve
   ! a consistent form for constructions across Transform subclasses.
   function new_CopyTransform(src_typekind, dst_typekind) result(transform)
      type(CopyTransform) :: transform
      type(ESMF_Typekind_Flag), intent(in) :: src_typekind
      type(ESMF_Typekind_Flag), intent(in) :: dst_typekind

      transform%src_typekind = src_typekind
      transform%dst_typekind = dst_typekind

   end function new_CopyTransform

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(CopyTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      ! No-op

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      use esmf
      class(CopyTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_StateItem_Flag) :: importType, exportType
      type(ESMF_Field) :: importField, exportField
      type(ESMF_FieldBundle) :: importBundle, exportBundle

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, itemType=importType, _RC)
      call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, itemType=exportType, _RC)
      _ASSERT(importType == exportType, 'The state items are differt types.')

      if(importType == MAPL_STATEITEM_FIELD) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, field=importField, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=exportField, _RC)
         call FieldCopy(importField, exportField, _RC)
         _RETURN(_SUCCESS)
      end if

      _ASSERT(importType == MAPL_STATEITEM_FIELDBUNDLE, 'Unsupported ESMF_State item type.') 

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldbundle=importBundle, _RC)
      call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, fieldbundle=exportBundle, _RC)
      call bundle_types_valid(importBundle, exportBundle, _RC)
      call FieldBundleCopy(importBundle, exportBundle, _RC)
      _RETURN(_SUCCESS)
      
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(this)

   end subroutine update

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(CopyTransform), intent(in) :: this

      id = TYPEKIND_TRANSFORM_ID
      _UNUSED_DUMMY(this)

   end function get_transformId

end module mapl3g_CopyTransform

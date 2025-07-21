#include "MAPL.h"

! A copy might be between different kinds and precisions, so is really
! a converter.  But ... what is a better name.
module mapl3g_CopyTransform
   use mapl3g_TransformId
   use mapl3g_ExtensionTransform
   use mapl_ErrorHandling
   use esmf
   use MAPL_FieldUtils
   implicit none

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
      type(ESMF_Field) :: f_in, f_out

      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)

      call FieldCopy(f_in, f_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine update

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(CopyTransform), intent(in) :: this

      id = TYPEKIND_TRANSFORM_ID
   end function get_transformId

end module mapl3g_CopyTransform

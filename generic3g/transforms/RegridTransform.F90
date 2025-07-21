#include "MAPL.h"

module mapl3g_RegridTransform
   use mapl3g_TransformId
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl3g_ExtensionTransform
   use mapl3g_TransformId
   use mapl3g_regridder_mgr
   use mapl3g_StateItem
   use mapl_ErrorHandling
   use esmf

   implicit none(type,external)
   private

   public :: RegridTransform

   type, extends(ExtensionTransform) :: ScalarRegridTransform
      type(ESMF_Geom) :: src_geom
      type(ESMF_Geom) :: dst_geom
      type(EsmfRegridderParam) :: dst_param

      class(Regridder), pointer :: regrdr
   contains
      procedure :: initialize
      procedure :: update
      procedure :: change_geoms
      procedure :: get_transformId
   end type ScalarRegridTransform

   interface RegridTransform
      module procedure :: new_ScalarRegridTransform
   end interface RegridTransform

contains

   function new_ScalarRegridTransform(src_geom, dst_geom, dst_param) result(transform)
      type(ScalarRegridTransform) :: transform
      type(ESMF_Geom), intent(in) :: src_geom
      type(ESMF_Geom), intent(in) :: dst_geom
      type(EsmfRegridderParam), intent(in) :: dst_param

      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager

      transform%src_geom = src_geom
      transform%dst_geom = dst_geom
      transform%dst_param = dst_param

   end function new_ScalarRegridTransform

   subroutine change_geoms(this, src_geom, dst_geom)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: src_geom
      type(ESMF_Geom), intent(in) :: dst_geom
      this%src_geom = src_geom
      this%dst_geom = dst_geom
      
   end subroutine change_geoms

   subroutine initialize(this, importState, exportState, clock, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager

      regridder_manager => get_regridder_manager()

      this%src_geom = get_geom(importState, 'import[1]')
      this%dst_geom = get_geom(exportState, 'export[1]')
      spec = RegridderSpec(this%dst_param, this%src_geom, this%dst_geom)
      this%regrdr => regridder_manager%get_regridder(spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(clock)

   contains

      function get_geom(state, itemName, rc) result(geom)
         type(ESMF_State), intent(inout) :: state
         character(*), intent(in) :: itemName
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_StateItem_Flag) :: itemType
         type(ESMF_Field) :: f
         type(ESMF_FieldBundle) :: fb
         type(ESMF_Geom) :: geom

         call ESMF_StateGet(state, itemName, itemType=itemType, _RC)
         if (itemType == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, itemName, field=f, _RC)
            call MAPL_FieldGet(f, geom=geom, _RC)
         elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, itemName, fieldBundle=fb, _RC)
            call MAPL_FieldBundleGet(fb, geom=geom, _RC)
         else
            _FAIL('unsupported itemType')
         end if

         _RETURN(_SUCCESS)
      end function get_geom
   end subroutine initialize


   subroutine update(this, importState, exportState, clock, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out
      type(ESMF_FieldBundle) :: fb_in, fb_out
      type(ESMF_StateItem_Flag) :: itemType_in, itemType_out

      call ESMF_StateGet(importState, itemName='import[1]', itemType=itemType_in, _RC)
      call ESMF_StateGet(importState, itemName='import[1]', itemType=itemType_out, _RC)

      _ASSERT(itemType_in == itemType_out, 'Regridder requires same itemType for input and output.')

      if (itemType_in == MAPL_STATEITEM_FIELD) then
         call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
         call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)
         call this%regrdr%regrid(f_in, f_out, _RC)
      else ! bundle case
         call ESMF_StateGet(importState, itemName='import[1]', fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName='export[1]', fieldBundle=fb_out, _RC)
         call this%regrdr%regrid(fb_in, fb_out, _RC)
      end if


      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine update

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(ScalarRegridTransform), intent(in) :: this

      id = GEOM_TRANSFORM_ID
   end function get_transformId

end module mapl3g_RegridTransform

#include "MAPL.h"

module mapl3g_RegridTransform
   use mapl3g_TransformId
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl3g_ExtensionTransform
   use mapl3g_TransformId
   use mapl3g_regridder_mgr
   use mapl3g_StateItem
   use mapl3g_ExtensionTransformUtils, only: bundle_types_valid
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
      procedure :: update_transform
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
      type(ESMF_Geom), optional, intent(in) :: src_geom
      type(ESMF_Geom), optional, intent(in) :: dst_geom
      if (present(src_geom)) this%src_geom = src_geom
      if (present(dst_geom)) this%dst_geom = dst_geom
      
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

      this%src_geom = get_geom(importState, COUPLER_IMPORT_NAME)
      this%dst_geom = get_geom(exportState, COUPLER_EXPORT_NAME)
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
      type(ESMF_Geom) :: geom_in, geom_out
      logical :: do_transform
      type(FieldBundleType_Flag) :: field_bundle_type

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, itemType=itemType_in, _RC)
      call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, itemType=itemType_out, _RC)

      _ASSERT(itemType_in == itemType_out, 'Regridder requires same itemType for input and output.')

      if (itemType_in == MAPL_STATEITEM_FIELD) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, field=f_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=f_out, _RC)
         call ESMF_FieldGet(f_in, geom=geom_in, _RC)
         call ESMF_FieldGet(f_out, geom=geom_out, _RC)
         call this%update_transform(geom_in, geom_out)
         call this%regrdr%regrid(f_in, f_out, _RC)
      else ! bundle case
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, fieldBundle=fb_out, _RC)
         call bundle_types_valid(fb_in, fb_out, _RC)
         call MAPL_FieldBundleGet(fb_in, geom=geom_in, _RC)
         call MAPL_FieldBundleGet(fb_out, geom=geom_out, _RC)
         call this%update_transform(geom_in, geom_out)
         do_transform = .true.
         call MAPL_FieldBundleGet(fb_in, fieldBundleType= field_bundle_type, _RC)
         if (field_bundle_type == FIELDBUNDLETYPE_BRACKET .or. field_bundle_type == FIELDBUNDLETYPE_VECTOR_BRACKET) then 
            call MAPL_FieldBundleGet(fb_in, bracket_updated=do_transform, _RC)
         end if
         if (do_transform) then
            call this%regrdr%regrid(fb_in, fb_out, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine update

   subroutine update_transform(this, src_geom, dst_geom, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: src_geom
      type(ESMF_Geom), intent(in) :: dst_geom
      integer, optional, intent(out) :: rc

      logical :: scr_geom_changed, dst_geom_changed
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      integer :: status

      scr_geom_changed = ESMF_GEOMMATCH_GEOMALIAS /= ESMF_GeomMatch(src_geom, this%src_geom)
      dst_geom_changed = ESMF_GEOMMATCH_GEOMALIAS /= ESMF_GeomMatch(dst_geom, this%dst_geom)
      if (scr_geom_changed) call this%change_geoms(src_geom=src_geom)      
      if (dst_geom_changed) call this%change_geoms(dst_geom=dst_geom)
      if (scr_geom_changed .or. dst_geom_changed) then
         regridder_manager => get_regridder_manager()
         spec = RegridderSpec(this%dst_param, this%src_geom, this%dst_geom)
         this%regrdr => regridder_manager%get_regridder(spec, _RC)
      end if
      _RETURN(_SUCCESS)
   end subroutine update_transform

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(ScalarRegridTransform), intent(in) :: this

      id = GEOM_TRANSFORM_ID
   end function get_transformId

end module mapl3g_RegridTransform

#include "MAPL_Generic.h"

module mapl3g_RegridTransform

   use mapl3g_ExtensionTransform
   use mapl3g_regridder_mgr
   use mapl_ErrorHandling
   use esmf

   implicit none
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
      spec = RegridderSpec(this%dst_param, this%src_geom, this%dst_geom)
      this%regrdr => regridder_manager%get_regridder(spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize


   subroutine update(this, importState, exportState, clock, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)
      call this%regrdr%regrid(f_in, f_out, _RC)


      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine update

end module mapl3g_RegridTransform

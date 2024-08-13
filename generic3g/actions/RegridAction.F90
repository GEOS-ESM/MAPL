#include "MAPL_Generic.h"

module mapl3g_RegridAction

   use mapl3g_ExtensionAction
   use mapl3g_regridder_mgr
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: RegridAction

   type, extends(ExtensionAction) :: ScalarRegridAction
      type(ESMF_Geom) :: src_geom
      type(ESMF_Geom) :: dst_geom
      type(EsmfRegridderParam) :: dst_param

      class(Regridder), pointer :: regrdr
      ! old
      type(ESMF_Field) :: f_src, f_dst
   contains
      procedure :: initialize
      procedure :: run
   end type ScalarRegridAction

!#   type, extends(AbstractAction) :: VectorRegridAction
!#      class(AbstractRegridder), pointer :: regridder
!#      type(ESMF_Field) :: uv_src(2), uv_dst(2)
!#   contains
!#      procedure :: run
!#   end type VectorRegridAction

   interface RegridAction
      module procedure :: new_ScalarRegridAction
      module procedure :: new_ScalarRegridAction2
!#      module procedure :: new_RegridAction_vector
!#      module procedure :: new_RegridAction_bundle
   end interface RegridAction

contains

   function new_ScalarRegridAction(geom_src, f_src, geom_dst, f_dst, param_dst, rc) result (action)
      type(ScalarRegridAction) :: action
      type(ESMF_Geom), intent(in) :: geom_src
      type(ESMF_Field), intent(in) :: f_src
      type(ESMF_Geom), intent(in) :: geom_dst
      type(ESMF_Field), intent(in) :: f_dst
      type(EsmfRegridderParam), intent(in) :: param_dst
      integer, optional, intent(out) :: rc

      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      integer :: status

      regridder_manager => get_regridder_manager()
      spec = RegridderSpec(param_dst, geom_src, geom_dst)
      action%regrdr => regridder_manager%get_regridder(spec, rc=status)

      action%f_src = f_src
      action%f_dst = f_dst

   end function new_ScalarRegridAction

  function new_ScalarRegridAction2(src_geom, dst_geom, dst_param) result(action)
      type(ScalarRegridAction) :: action
      type(ESMF_Geom), intent(in) :: src_geom
      type(ESMF_Geom), intent(in) :: dst_geom
      type(EsmfRegridderParam), intent(in) :: dst_param

      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      integer :: status

      action%src_geom = src_geom
      action%dst_geom = dst_geom
      action%dst_param = dst_param

   end function new_ScalarRegridAction2

!#   function new_RegridAction_vector(uv_src, uv_dst) then (action)
!#      use mapl_RegridderManager
!#
!#      ptype(ESMF_Grid) :: grid_src, grid_dst
!#
!#      action%uv_src = uv_src
!#      action%uv_dst = uv_dst
!#
!#      get_grid(grid_src)
!#      get_grid(grid_dst)
!#      action%regridder => regridder_manager%get_regridder(grid_src, grid_dst)
!#
!#   end function new_RegridAction_scalar
!#
!#
 
   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(ScalarRegridAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager

      regridder_manager => get_regridder_manager()
      spec = RegridderSpec(this%dst_param, this%src_geom, this%dst_geom)
      this%regrdr => regridder_manager%get_regridder(spec, rc=status)

      _RETURN(_SUCCESS)
   end subroutine initialize


   subroutine run(this, importState, exportState, clock, rc)
      use esmf
      class(ScalarRegridAction), intent(inout) :: this
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
   end subroutine run

!#   subroutine run_vector(this, importState, exporState)
!#
!#      call get_pointer(importState, fname_src_u, f_src(1))
!#      call get_pointer(importState, fname_src_v, f_src(2)
!#      call get_pointer(exportState, fname_dst_u, f_dst(1))
!#      call get_pointer(exportState, fname_dst_v, f_dst(2))
!#
!#      call regridder%regrid(f_src(:), f_dst(:), _RC)
!#
!#   end subroutine run

!#   subroutine run_bundle(this)
!#
!#      call this%regridder%regrid(this%b_src, this%b_dst, _RC)
!#
!#   end subroutine run
!#
end module mapl3g_RegridAction

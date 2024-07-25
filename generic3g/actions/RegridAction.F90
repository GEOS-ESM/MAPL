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
      class(Regridder), pointer :: regrdr
      type(ESMF_Field) :: f_src, f_dst
   contains
      procedure :: run => run_scalar
   end type ScalarRegridAction

!#   type, extends(AbstractAction) :: VectorRegridAction
!#      class(AbstractRegridder), pointer :: regridder
!#      type(ESMF_Field) :: uv_src(2), uv_dst(2)
!#   contains
!#      procedure :: run
!#   end type VectorRegridAction

   interface RegridAction
      module procedure :: new_ScalarRegridAction
!#      module procedure :: new_RegridAction_vector
!#      module procedure :: new_RegridAction_bundle
   end interface RegridAction
      
contains

   function new_ScalarRegridAction(geom_src, f_src, param_src, geom_dst, f_dst, param_dst) result (action)
      type(ScalarRegridAction) :: action
      type(ESMF_Geom), intent(in) :: geom_src
      type(ESMF_Field), intent(in) :: f_src
      type(EsmfRegridderParam), intent(in) :: param_src
      type(ESMF_Geom), intent(in) :: geom_dst
      type(ESMF_Field), intent(in) :: f_dst
      type(EsmfRegridderParam), intent(in) :: param_dst

      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      type(EsmfRegridderParam) :: param_to_use
      integer :: status

      regridder_manager => get_regridder_manager()
      param_to_use = choose_param_(param_src, param_dst)
      spec = RegridderSpec(param_to_use, geom_src, geom_dst)
      action%regrdr => regridder_manager%get_regridder(spec, rc=status)

      action%f_src = f_src
      action%f_dst = f_dst

   end function new_ScalarRegridAction

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
   subroutine run_scalar(this, rc)
      class(ScalarRegridAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: f_src, f_dst
      integer :: status

      call this%regrdr%regrid(this%f_src, this%f_dst, _RC)
      _RETURN(_SUCCESS)
   end subroutine run_scalar

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

   function choose_param_(param_src, param_dst, rc) result(param)
      type(EsmfRegridderParam) :: param
      type(EsmfRegridderParam), intent(in) :: param_src
      type(EsmfRegridderParam), intent(in) :: param_dst
      integer, optional, intent(out) :: rc

      _ASSERT(param_src == param_dst, "param_src /= param_dst")
      ! TODO: If both are null, use EsmfRegridderParam() in the next step??
      param = param_src
   end function choose_param_
      
end module mapl3g_RegridAction

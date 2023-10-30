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
      type(ESMF_Field) :: f_in, f_out
   contains
      procedure :: run => run_scalar
   end type ScalarRegridAction

!#   type, extends(AbstractAction) :: VectorRegridAction
!#      class(AbstractRegridder), pointer :: regridder
!#      type(ESMF_Field) :: uv_in(2), uv_out(2)
!#   contains
!#      procedure :: run
!#   end type VectorRegridAction

   interface RegridAction
      module procedure :: new_ScalarRegridAction
!#      module procedure :: new_RegridAction_vector
!#      module procedure :: new_RegridAction_bundle
   end interface RegridAction
      
contains

   function new_ScalarRegridAction(geom_in, f_in, geom_out, f_out) result (action)
      type(ScalarRegridAction) :: action
      type(ESMF_Geom) :: geom_in
      type(ESMF_Field), intent(in) :: f_in
      type(ESMF_Geom) :: geom_out
      type(ESMF_Field), intent(in) :: f_out

      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      integer :: status

      regridder_manager => get_regridder_manager()
      spec = RegridderSpec(EsmfRegridderParam(), geom_in, geom_out)
      action%regrdr => regridder_manager%get_regridder(spec, rc=status)

      action%f_in = f_in
      action%f_out = f_out

   end function new_ScalarRegridAction

!#   function new_RegridAction_vector(uv_in, uv_out) then (action)
!#      use mapl_RegridderManager
!#
!#      ptype(ESMF_Grid) :: grid_in, grid_out
!#
!#      action%uv_in = uv_in
!#      action%uv_out = uv_out
!#
!#      get_grid(grid_in)
!#      get_grid(grid_out)
!#      action%regridder => regridder_manager%get_regridder(grid_in, grid_out)
!#
!#   end function new_RegridAction_scalar
!#
!#   
   subroutine run_scalar(this, rc)
      class(ScalarRegridAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: f_in, f_out
      integer :: status

      call this%regrdr%regrid(this%f_in, this%f_out, _RC)
      _RETURN(_SUCCESS)
   end subroutine run_scalar

!#   subroutine run_vector(this, importState, exporState)
!#
!#      call get_pointer(importState, fname_in_u, f_in(1))
!#      call get_pointer(importState, fname_in_v, f_in(2)
!#      call get_pointer(exportState, fname_out_u, f_out(1))
!#      call get_pointer(exportState, fname_out_v, f_out(2))
!#
!#      call regridder%regrid(f_in(:), f_out(:), _RC)
!#
!#   end subroutine run

!#   subroutine run_bundle(this)
!#
!#      call this%regridder%regrid(this%b_in, this%b_out, _RC)
!#
!#   end subroutine run
!#
end module mapl3g_RegridAction
      

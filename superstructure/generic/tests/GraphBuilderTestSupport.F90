#include "MAPL.h"

! Test-only helper module shared across multiple `.pf` files in this
! directory (mirrors the precedent set by
! superstructure/generic/graph/tests/IdGeneratorTestSupport.F90: plain
! Fortran module, not a pFUnit test file itself).
!
! Provides the fixtures the GraphBuilder and hierarchy tests share:
! `make_outer_meta` builds a top-level OuterMetaComponent without the
! SetServices/DSO machinery, and `make_var` builds a VariableSpec from a
! state intent and short name (`units` optional). See
! docs/graph/spec/20-implementation-roadmap.md sec 20.4.17.
module GraphBuilderTestSupport_mod
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent, attach_outer_meta, get_outer_meta
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_VariableSpec_mod, only: VariableSpec
   use mapl_geom_api, only: mapl_new_GeomId
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_NONE
   use mapl_ErrorHandling_mod
   use esmf
   implicit none
   private

   public :: make_outer_meta
   public :: make_var

contains

   ! Directly builds a top-level OuterMetaComponent, bypassing
   ! SetServices/DSO machinery.
   subroutine make_outer_meta(name, outer_gc, outer_meta, rc)
      character(*), intent(in) :: name
      type(ESMF_GridComp), intent(out) :: outer_gc
      type(OuterMetaComponent), pointer, intent(out) :: outer_meta
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: user_gc
      type(GriddedComponentDriver) :: user_gc_driver
      type(ESMF_HConfig) :: hconfig

      hconfig = ESMF_HConfigCreate(content='{}', _RC)
      outer_gc = ESMF_GridCompCreate(name='['//trim(name)//']', _RC)
      user_gc = ESMF_GridCompCreate(name=name, _RC)
      user_gc_driver = GriddedComponentDriver(user_gc)

      call attach_outer_meta(outer_gc, _RC)
      outer_meta => get_outer_meta(outer_gc, _RC)
#ifndef __GFORTRAN__
      outer_meta = OuterMetaComponent(outer_gc, user_gc_driver, hconfig=hconfig)
#else
      block
         type(OuterMetaComponent) :: tmp
         tmp = OuterMetaComponent(outer_gc, user_gc_driver, hconfig=hconfig)
         outer_meta = tmp
      end block
#endif
      call outer_meta%init_meta(_RC)

      _RETURN(_SUCCESS)
   end subroutine make_outer_meta

   function make_var(state_intent, short_name, units) result(var_spec)
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      character(*), optional, intent(in) :: units
      type(VariableSpec) :: var_spec

      var_spec%state_intent = state_intent
      var_spec%short_name = short_name
      ! An item with no geometry is DEFERRED and its connection waits for a
      ! value, so these fixtures share one identity to wire. Tests of the
      ! wait itself (test_deferred_geometry_connection_waits_then_wires and
      ! similar) build their own DEFERRED VariableSpec instead.
      var_spec%geom_id = mapl_new_GeomId(1)
      ! Likewise a vertical coordinate is DEFERRED unless the item is 2D.
      var_spec%vertical_stagger = VERTICAL_STAGGER_NONE
      if (present(units)) var_spec%units = units
   end function make_var

end module GraphBuilderTestSupport_mod

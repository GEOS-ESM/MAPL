#include "MAPL.h"

! Task 4: Public ports and child proxies
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_PortsAndProxies_smod
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_NONE, operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! design.md Decisions "Geom/vgrid resolution order": (a) the export's
   ! own VariableSpec%geom if explicitly allocated (HistoryCollection-
   ! style explicit override), else (b) the owning OuterMetaComponent's
   ! component-wide default via its existing public has_geom()/get_geom()
   ! accessors (the same fallback advertise_variable.F90 already applies
   ! to every VariableSpec), else (c) not found - no cross-component
   ! mirror propagation (that remains a distinct, future capability).
   module subroutine resolve_export_geom(var_spec, owner, geom, found, rc)
      type(VariableSpec), intent(in) :: var_spec
      class(OuterMetaComponent), target, intent(inout) :: owner
      type(ESMF_Geom), intent(out) :: geom
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status

      found = .false.

      if (allocated(var_spec%geom)) then
         geom = var_spec%geom
         found = .true.
         _RETURN(_SUCCESS)
      end if

      if (owner%has_geom()) then
         geom = owner%get_geom(_RC)
         found = .true.
      end if

      _RETURN(_SUCCESS)
   end subroutine resolve_export_geom

   ! Same priority order as resolve_export_geom() above, for the
   ! vertical grid - but unlike geom, "not found" here means "no vertical
   ! dimension" (a legitimate 2D field), not a failure; see
   ! try_materialize_field_extension()'s own comment. An explicit
   ! `vertical_stagger == VERTICAL_STAGGER_NONE` (the parser's own
   ! `vertical_dim_spec: NONE`, VariableSpec.F90) is this VariableSpec's
   ! own explicit declaration that the field has no vertical dimension
   ! at all - honored ahead of the component-wide default so a 2D field
   ! declared inside an otherwise-3D-capable component is not
   ! mistakenly given that component's own vertical grid.
   module subroutine resolve_export_vgrid(var_spec, owner, vgrid)
      type(VariableSpec), intent(in) :: var_spec
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(VerticalGrid), allocatable, intent(out) :: vgrid

      class(VerticalGrid), pointer :: component_vgrid

      if (allocated(var_spec%vertical_stagger)) then
         if (var_spec%vertical_stagger == VERTICAL_STAGGER_NONE) return
      end if

      if (allocated(var_spec%vertical_grid)) then
         vgrid = var_spec%vertical_grid
         return
      end if

      component_vgrid => owner%get_vertical_grid()
      if (associated(component_vgrid)) vgrid = component_vgrid
   end subroutine resolve_export_vgrid

end submodule GraphBuilder_PortsAndProxies_smod

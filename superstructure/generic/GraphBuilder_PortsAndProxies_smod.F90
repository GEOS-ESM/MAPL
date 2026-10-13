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
   ! own GraphStateItem%declared_geom if explicitly set at advertise time
   ! (HistoryCollection-style explicit override, captured once from
   ! var_spec%geom by advertise_one - design.md correction), else (b) the
   ! owning OuterMetaComponent's component-wide default via its existing
   ! public has_geom()/get_geom() accessors (the same fallback
   ! advertise_variable.F90 already applies to every VariableSpec), else
   ! (c) not found - no cross-component mirror propagation (that remains
   ! a distinct, future capability). export_item is read-only here (the
   ! export's own already-advertised GraphStateItem, by value - no
   ! VariableSpec reference needed, proposal.md's core claim).
   module subroutine resolve_export_geom(export_item, owner, geom, found, rc)
      type(GraphStateItem), intent(in) :: export_item
      class(OuterMetaComponent), target, intent(inout) :: owner
      type(ESMF_Geom), intent(out) :: geom
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom), allocatable :: declared_geom

      found = .false.

      call export_item%get_declared_geom(declared_geom)
      if (allocated(declared_geom)) then
         geom = declared_geom
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
   ! `vertical_dim_spec: NONE`, VariableSpec.F90, captured on
   ! export_item%get_vertical_stagger() at advertise time) is this
   ! item's own explicit declaration that the field has no vertical
   ! dimension at all - honored ahead of the component-wide default so a
   ! 2D field declared inside an otherwise-3D-capable component is not
   ! mistakenly given that component's own vertical grid.
   module subroutine resolve_export_vgrid(export_item, owner, vgrid)
      type(GraphStateItem), intent(in) :: export_item
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(VerticalGrid), allocatable, intent(out) :: vgrid

      class(VerticalGrid), pointer :: component_vgrid
      type(VerticalStaggerLoc), allocatable :: declared_vertical_stagger
      class(VerticalGrid), allocatable :: declared_vertical_grid

      call export_item%get_vertical_stagger(declared_vertical_stagger)
      if (allocated(declared_vertical_stagger)) then
         if (declared_vertical_stagger == VERTICAL_STAGGER_NONE) return
      end if

      call export_item%get_declared_vertical_grid(declared_vertical_grid)
      if (allocated(declared_vertical_grid)) then
         vgrid = declared_vertical_grid
         return
      end if

      component_vgrid => owner%get_vertical_grid()
      if (associated(component_vgrid)) vgrid = component_vgrid
   end subroutine resolve_export_vgrid

end submodule GraphBuilder_PortsAndProxies_smod

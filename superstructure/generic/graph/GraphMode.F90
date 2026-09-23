!------------------------------------------------------------------------------
! mapl_GraphMode_mod: single, global, default-off switch selecting
! whether real MAPL behavior is driven by legacy StateRegistry-based
! resolution (the default, unchanged behavior of every production run
! today) or by the graph-native ComponentGraph/GraphBuilder machinery
! under development (docs/graph/spec/20-implementation-roadmap.md).
!
! Precedent: mirrors mapl_ExtensionResolution_mod's own
! materialize_extensions/set_materialize_extensions/
! materialize_extensions_enabled flag exactly in shape (module-level
! logical, save, default .false.; a setter; a query function) - that
! flag gates one narrow decision (real payload materialization for an
! extension chain); this one is the broader, framework-wide analog
! anticipated by openspec/changes/horizontal-geometry-graph-state-item's
! design.md: as graph-native code starts making real decisions (not just
! building parallel, additive, structure-only representation), it will
! increasingly conflict with the OuterMetaComponent/StateRegistry layer
! it runs alongside (e.g. GraphBuilder's geometry hook deciding
! this%geom versus legacy initialize_geom_a/propagate_geom_to_children
! deciding it; a future graph-native state-population step versus
! StateRegistry%add_to_states) - this flag is the single point that
! decides who wins, one call site at a time, as each such conflict is
! reached and explicitly gated.
!
! Off (.false.) by default: every real production run behaves exactly as
! it did before this module existed, unless something explicitly calls
! set_graph_native_enabled(.true.) - today that is only test code and,
! per-call-site, whichever specific graph-native behaviors have been
! wired to consult this flag (see each call site's own comment for
! exactly what it gates - this module makes no claim that every
! legacy/graph-native conflict in MAPL already checks it).
!------------------------------------------------------------------------------
module mapl_GraphMode_mod
   implicit none(type, external)
   private

   public :: set_graph_native_enabled
   public :: graph_native_enabled

   logical, save :: graph_native = .false.

contains

   ! Test-only (and future real-config-driven) setter. No production
   ! initialization code calls this today.
   subroutine set_graph_native_enabled(enabled)
      logical, intent(in) :: enabled

      graph_native = enabled
   end subroutine set_graph_native_enabled

   logical function graph_native_enabled() result(enabled)
      enabled = graph_native
   end function graph_native_enabled

end module mapl_GraphMode_mod

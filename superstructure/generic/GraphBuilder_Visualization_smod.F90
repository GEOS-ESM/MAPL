#include "MAPL.h"

! Task 6: Visualization enrichment (visualization-enrichment-layer change)
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_Visualization_smod
   use mapl_NodeLabel_mod, only: NodeLabel
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Builds a NodeId -> label lookup for THIS component's own local
   ! graph only (design.md Goals/Non-Goals): a component's own
   ! advertised items are labeled by their own short_name (read from
   ! ComponentSpec%var_specs, the same iteration graphbuilder_advertise
   ! already performs).
   !
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 3.1/3.4:
   ! this used to also label each cached child-proxy node in this
   ! component's own graph ("<child_name>:<short_name>", marked
   ! is_proxy) - with proxy nodes retired (no parent-local stand-in is
   ! created anymore; a cross-component edge references the child's own
   ! real NodeId directly, in the child's own graph), there is nothing
   ! left for that section to label: this component's own
   ! ComponentGraph%get_node_ids() never contains a child's NodeId.
   ! GraphExport.F90 itself remains single-ComponentGraph-scoped
   ! (REQ-VIZ-005 hierarchy-wide export is still not implemented), so a
   ! child's own real node is simply outside this function's own
   ! single-graph view - not a regression this function can address.
   module function graphbuilder_build_label_map(this, rc) result(label_map)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeIdLabelMap) :: label_map

      type(ComponentGraph), pointer :: graph
      type(ComponentSpec), pointer :: comp_spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      character(:), allocatable :: key
      type(NodeId), pointer :: found_id

      graph => this%get_component_graph()
      comp_spec => this%get_component_spec()

      ! This component's own advertised items (design.md - "walking
      ! var_specs", not reversing the resource index).
      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            key = item_key(var_spec%state_intent, var_spec%short_name)
            found_id => graph%get_resource_index(key)
            if (associated(found_id)) then
               call label_map%insert(found_id, NodeLabel(var_spec%short_name, is_proxy=.false.))
            end if
         end do
      end associate

      _RETURN(_SUCCESS)
   end function graphbuilder_build_label_map

end submodule GraphBuilder_Visualization_smod

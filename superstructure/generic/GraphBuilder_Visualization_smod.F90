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
   ! already performs); each cached child-proxy node in this
   ! component's own graph is labeled "<child_name>:<short_name>" and
   ! marked as a proxy, read from that child's own already-published
   ! ComponentSpec%var_specs (the same framework-internal carve-out
   ! get_or_make_local_node_id already uses, REQ-GB-002) - never the
   ! child's own graph/NodeId/DependencyNetwork. A child item never
   ! actually connected has no cached proxy, so
   ! graph%get_resource_index(proxy_key(...)) simply returns
   ! unassociated and that item is left out of the map - not an error
   ! (design.md Decisions, "Enrichment builds the map by walking
   ! var_specs").
   module function graphbuilder_build_label_map(this, rc) result(label_map)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeIdLabelMap) :: label_map

      integer :: status
      integer :: i, num_children
      type(ComponentGraph), pointer :: graph
      type(ComponentSpec), pointer :: comp_spec, child_spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      character(:), allocatable :: child_name
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

      ! Each child's cached proxy nodes, if any, in THIS component's
      ! own graph (never the child's own graph - design.md "Enrichment
      ! layer does not cross into a child's own graph").
      num_children = this%get_num_children()
      do i = 1, num_children
         child_name = this%get_child_name(i, _RC)
         child_spec => this%get_child_component_spec(child_name, _RC)

         associate (ce => child_spec%var_specs%ftn_end())
            iter = child_spec%var_specs%ftn_begin()
            do while (iter /= ce)
               call iter%next()
               var_spec => iter%of()
               key = proxy_key(child_name, var_spec%state_intent, var_spec%short_name)
               found_id => graph%get_resource_index(key)
               if (associated(found_id)) then
                  call label_map%insert(found_id, &
                       NodeLabel(child_name // ':' // var_spec%short_name, is_proxy=.true.))
               end if
            end do
         end associate
      end do

      _RETURN(_SUCCESS)
   end function graphbuilder_build_label_map

end submodule GraphBuilder_Visualization_smod

#include "MAPL.h"

! Visualization enrichment: labels for graph export.

submodule (mapl_GraphBuilder_mod) GraphBuilder_Visualization_smod
   use mapl_NodeLabel_mod, only: NodeLabel
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Builds a NodeId -> label lookup for THIS component's own local
   ! graph only: its own advertised items, labeled by short_name.
   ! A child's NodeId never appears in this component's graph, and
   ! GraphExport is single-ComponentGraph-scoped (hierarchy-wide export
   ! is not implemented), so children are not labeled here.
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

      ! Walk var_specs rather than reversing the resource index.
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

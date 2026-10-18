#define I_AM_MAIN
#include "MAPL.h"

!------------------------------------------------------------------------------
! graph_diagnostic.x: standalone, hierarchy-wide graph diagnostic driver
! (docs/graph/spec/20-implementation-roadmap.md sec 20.4.21, Phase 21).
!
! Scope (read the roadmap section itself before changing this file):
! this is NOT a hierarchy-wide (multi-ComponentGraph, stitched-at-
! boundary) exporter - that is Q18's own open, harder question
! (docs/graph/spec/17-open-questions.md Q18), deliberately out of
! scope here. This driver instead walks the real OuterMetaComponent
! tree (get_num_children/get_child_name/get_child_outer_meta, the
! same walk GraphBuilder_Freeze_smod.F90's own freeze_subtree_ already
! uses) and calls the EXISTING, already-landed per-component
! graphbuilder_build_label_map + export_graph_dot + export_graph_json
! (GraphExport.F90/GraphBuilder_Visualization_smod.F90) once per
! component - "automate what a human would do by hand, once per
! component, across a whole real configuration," per the roadmap's
! own framing. One .dot/.json PAIR is written per component, named by
! that component's own qualified path (root/child/grandchild, the
! same '/' convention resolve_qualified_component/
! enumerate_qualified_exports already use elsewhere in
! GraphBuilder.F90) - never one stitched graph across component
! boundaries.
!
! Both DOT and JSON are emitted unconditionally for every component -
! per the roadmap's own explicit "audience-driven output requirement"
! (sec 20.4.21 item 3): a human looks at the (optionally
! render_graph.py-rendered) picture, an agent parses the JSON, in the
! same debugging session, against the same underlying export call.
!
! Bootstrap: reuses mapl_GraphScenarioBootstrap_mod%build_scenario_tree
! (Phase 21 sub-step 21-1), itself extracted from
! superstructure/generic/tests/Test_Scenarios.pf's own setup() - no
! second YAML-loading path invented for this driver.
!
! Usage (deliberately minimal - this is diagnostic tooling for
! debugging a scenario test, not a production CLI with a stable
! argument contract):
!
!   graph_diagnostic.x <config.yaml> <output_dir>
!
! Writes <output_dir>/<qualified_component_path>.dot and .json for
! every component in the tree rooted at <config.yaml> (with '/'
! replaced by '_' in the file name, since component paths may be
! multiple segments deep but a single flat output directory is all
! this diagnostic tool needs - see write_component_export's own
! comment).
!
! Macro note: only the top-level statements directly in the PROGRAM
! body below are compiled under I_AM_MAIN's _RETURN/_ASSERT/_VERIFY
! expansion (every one of those unconditionally calls MAPL_abort() on
! any non-success status, appropriate for a bare main program with no
! rc of its own to propagate). The internal subroutines in the
! CONTAINS section below are ordinary, non-top-level procedures that
! need _RETURN to behave as a normal RETURN-to-caller instead - so
! I_AM_MAIN is #undef'd and MAPL.h re-included immediately after
! CONTAINS, restoring the standard (non-main-program) macro
! expansions for every procedure defined below that point.
!------------------------------------------------------------------------------
program graph_diagnostic
   use MAPL
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use mapl_GraphScenarioBootstrap_mod, only: build_scenario_tree
   use mapl_GraphBuilder_mod, only: GraphBuilder
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_GraphExport_mod, only: export_graph_dot, export_graph_json
   use mapl_NodeIdLabelMap_mod, only: NodeIdLabelMap
   use mapl_ErrorHandling_mod
   use esmf
   implicit none(type, external)

   integer :: status
   character(:), allocatable :: config_file, output_dir
   type(ESMF_GridComp) :: outer_gc
   type(OuterMetaComponent), pointer :: outer_meta
   integer :: n_written

   call parse_arguments(config_file, output_dir, _RC)

   call MAPL_Initialize()

   call build_scenario_tree(config_file, outer_gc, outer_meta, _RC)

   n_written = 0
   call export_subtree(outer_meta, root_path='', output_dir=output_dir, n_written=n_written, _RC)

   write(*, '(A,I0,A,A)') 'graph_diagnostic: wrote ', n_written, &
        ' component .dot/.json pair(s) to ', output_dir

   call MAPL_Finalize()

contains

#undef I_AM_MAIN
#include "MAPL.h"

   subroutine parse_arguments(config_file, output_dir, rc)
      character(:), allocatable, intent(out) :: config_file
      character(:), allocatable, intent(out) :: output_dir
      integer, optional, intent(out) :: rc

      integer :: nargs
      character(len=4096) :: buffer

      nargs = command_argument_count()
      if (nargs /= 2) then
         write(0, '(A)') 'Usage: graph_diagnostic.x <config.yaml> <output_dir>'
         _ASSERT(.false., 'graph_diagnostic.x requires exactly 2 arguments: <config.yaml> <output_dir>')
      end if

      call get_command_argument(1, buffer)
      config_file = trim(buffer)
      call get_command_argument(2, buffer)
      output_dir = trim(buffer)

      call make_directory(output_dir, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_arguments

   ! mkdir -p equivalent via EXECUTE_COMMAND_LINE - no new Fortran
   ! directory-creation dependency pulled in for one diagnostic tool;
   ! same pragmatic approach as other MAPL utilities that need an
   ! output directory to exist (e.g. checkpoint directories created by
   ! shelling out, see RestartHandler.F90 precedent for 'mkdir -p'
   ! usage elsewhere in this codebase).
   subroutine make_directory(path, rc)
      character(*), intent(in) :: path
      integer, optional, intent(out) :: rc

      integer :: exit_status, cmd_status

      call execute_command_line('mkdir -p "' // path // '"', &
           exitstat=exit_status, cmdstat=cmd_status)
      _ASSERT(cmd_status == 0 .and. exit_status == 0, 'graph_diagnostic: failed to create output directory: ' // path)

      _RETURN(_SUCCESS)
   end subroutine make_directory

   ! Recursive whole-tree walk, mirroring
   ! GraphBuilder_Freeze_smod.F90's own freeze_subtree_ pattern exactly
   ! (get_num_children/get_child_name/get_child_outer_meta) - an
   ! ordinary explicit Fortran recursion, not an ESMF phase dispatch.
   ! Per-component only: NO hierarchy-wide stitching across component
   ! boundaries (Q18, deliberately out of scope - see this file's own
   ! header comment).
   recursive subroutine export_subtree(meta, root_path, output_dir, n_written, rc)
      type(OuterMetaComponent), target, intent(inout) :: meta
      character(*), intent(in) :: root_path
      character(*), intent(in) :: output_dir
      integer, intent(inout) :: n_written
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: this_path, child_name, child_path
      integer :: i, num_children
      type(OuterMetaComponent), pointer :: child_meta

      this_path = root_path

      call write_component_export(meta, this_path, output_dir, _RC)
      n_written = n_written + 1

      num_children = meta%get_num_children()
      do i = 1, num_children
         child_name = meta%get_child_name(i, _RC)
         child_meta => meta%get_child_outer_meta(child_name, _RC)

         if (len(this_path) == 0) then
            child_path = child_name
         else
            child_path = this_path // '/' // child_name
         end if

         call export_subtree(child_meta, child_path, output_dir, n_written, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine export_subtree

   ! Exports ONE component's own local ComponentGraph (REQ-VIZ-003:
   ! graph-neutral exporter, single-ComponentGraph-scoped by design,
   ! REQ-VIZ-005/Q18 hierarchy-wide stitching deliberately not
   ! attempted) to a .dot/.json pair, enriched with this component's
   ! own build_label_map (REQ-VIZ-004, GraphBuilder_Visualization_
   ! smod.F90) - the exact "build_label_map + export_graph_dot +
   ! export_graph_json once per component" sequence the roadmap names.
   !
   ! File naming: qualified_path with '/' -> '_' (a multi-segment
   ! component path, e.g. 'comp_mid/comp_dst', becomes
   ! 'comp_mid_comp_dst.dot') written flat into output_dir - a human
   ! or agent can tell which file belongs to which component in the
   ! hierarchy directly from the file name, without needing nested
   ! subdirectories for this diagnostic tool's own one-shot use case.
   ! The empty root_path (this component IS the tree root) is named
   ! 'root'.
   subroutine write_component_export(meta, qualified_path, output_dir, rc)
      type(OuterMetaComponent), target, intent(inout) :: meta
      character(*), intent(in) :: qualified_path
      character(*), intent(in) :: output_dir
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphBuilder) :: gb
      type(ComponentGraph), pointer :: graph
      type(NodeIdLabelMap) :: label_map
      character(:), allocatable :: dot_text, json_text
      character(:), allocatable :: file_stub, dot_path, json_path
      integer :: dot_unit, json_unit
      integer :: io_status

      graph => meta%get_component_graph()
      label_map = gb%build_label_map(meta, _RC)

      dot_text = export_graph_dot(graph, include_revisions=.true., label_map=label_map, _RC)
      json_text = export_graph_json(graph, include_revisions=.true., label_map=label_map, _RC)

      file_stub = qualified_path
      if (len(file_stub) == 0) file_stub = 'root'
      file_stub = replace_char(file_stub, '/', '_')

      dot_path = trim(output_dir) // '/' // file_stub // '.dot'
      json_path = trim(output_dir) // '/' // file_stub // '.json'

      open(newunit=dot_unit, file=dot_path, status='replace', action='write', iostat=io_status)
      _ASSERT(io_status == 0, 'graph_diagnostic: failed to open for write: ' // dot_path)
      write(dot_unit, '(A)') dot_text
      close(dot_unit)

      open(newunit=json_unit, file=json_path, status='replace', action='write', iostat=io_status)
      _ASSERT(io_status == 0, 'graph_diagnostic: failed to open for write: ' // json_path)
      write(json_unit, '(A)') json_text
      close(json_unit)

      write(*, '(A,A,A,A,A)') '  component "', trim(qualified_path), '" -> ', file_stub, '.{dot,json}'

      _RETURN(_SUCCESS)
   end subroutine write_component_export

   function replace_char(text, from, to) result(replaced)
      character(*), intent(in) :: text
      character(1), intent(in) :: from
      character(1), intent(in) :: to
      character(:), allocatable :: replaced

      integer :: i

      replaced = text
      do i = 1, len(replaced)
         if (replaced(i:i) == from) replaced(i:i) = to
      end do
   end function replace_char

end program graph_diagnostic

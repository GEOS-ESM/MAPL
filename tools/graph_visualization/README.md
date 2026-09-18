# graph_visualization

`render_graph.py` renders a Graphviz `.dot` file to an image via the
system's installed Graphviz `dot` executable. Standard library only -
no additional Python package required (beyond having Graphviz's `dot`
on `PATH`).

## End-to-end flow

1. A Fortran component's local `ComponentGraph` is exported to a `.dot`
   file via `superstructure/generic/graph/GraphExport.F90`'s
   `export_graph_dot` (optionally enriched with human-readable
   import/export/internal names and child-proxy markers via
   `superstructure/generic/GraphBuilder.F90`'s `build_label_map` - see
   `openspec/changes/visualization-enrichment-layer`).
2. Render that `.dot` file to an image:

   ```sh
   python3 tools/graph_visualization/render_graph.py graph.dot graph.svg
   ```

   The output format (`svg`, `png`, ...) is inferred from the output
   path's extension.

## Tests

```sh
python3 -m unittest discover -s tools/graph_visualization/tests
```

The successful-render test is skipped (not failed) on a machine
without Graphviz's `dot` installed; the missing-`dot` and
malformed-input cases are exercised via `subprocess.run` mocking and
do not require Graphviz to be present.

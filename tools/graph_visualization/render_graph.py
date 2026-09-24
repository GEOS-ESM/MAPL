#!/usr/bin/env python3
"""render_graph.py: render a Graphviz .dot file to an image.

Standalone, non-Fortran companion to graph/visualization-export
(superstructure/generic/graph/GraphExport.F90): turns a .dot file
produced by that exporter (optionally enriched with human-readable
labels/proxy markers by GraphBuilder.F90's build_label_map, see
graph/visualization-enrichment-layer) into a rendered image, per
REQ-VIZ-002's "delegated to established external tools" posture.

Shells out to the system's installed Graphviz `dot` executable via
`subprocess` - no third-party Python package dependency (stdlib only).
DOT input only; JSON export is a different, not-yet-needed consumer
(REQ-VIZ-009) - see openspec/changes/visualization-enrichment-layer/
design.md, "Render script shells out to the `dot` binary via
subprocess".

Usage:
    render_graph.py <input.dot> <output.svg|output.png|...>

The output image format is inferred from the output path's file
extension (passed straight through to `dot -T<fmt>`).
"""
import argparse
import os
import subprocess
import sys


class RenderError(Exception):
    """Raised for any failure this script itself detects and reports
    cleanly (as opposed to letting a raw traceback surface)."""


def output_format_from_path(output_path):
    """Graphviz output format (`-T` argument) inferred from the output
    path's file extension, e.g. 'graph.svg' -> 'svg'."""
    _, ext = os.path.splitext(output_path)
    fmt = ext.lstrip('.').lower()
    if not fmt:
        raise RenderError(
            "Cannot infer an output format from '%s' - the output path "
            "needs a file extension (e.g. .svg, .png)." % output_path)
    return fmt


def render(input_path, output_path, dot_executable='dot'):
    """Renders input_path (a .dot file) to output_path via the
    installed Graphviz `dot` executable. Raises RenderError on any
    failure - missing `dot`, or `dot` itself reporting an error - never
    leaving a partial/misleading output file behind on failure.
    """
    fmt = output_format_from_path(output_path)

    try:
        completed = subprocess.run(
            [dot_executable, '-T' + fmt, input_path, '-o', output_path],
            capture_output=True, text=True)
    except FileNotFoundError:
        raise RenderError(
            "Graphviz's 'dot' executable could not be found on PATH. "
            "Install Graphviz (e.g. 'brew install graphviz' or "
            "'apt-get install graphviz') and try again.")

    if completed.returncode != 0:
        # dot does not reliably create a (possibly truncated) output
        # file on failure, but guard against it anyway - a failed
        # render must never leave behind a file that implies success.
        if os.path.exists(output_path):
            os.remove(output_path)
        message = completed.stderr.strip() or completed.stdout.strip()
        raise RenderError(
            "Graphviz 'dot' failed to render '%s' (exit code %d):\n%s"
            % (input_path, completed.returncode, message))


def parse_args(argv=None):
    p = argparse.ArgumentParser(
        description="Render a .dot file (from graph/visualization-export) "
                    "to an image via the installed Graphviz 'dot' executable.")
    p.add_argument('input', help='input .dot file path')
    p.add_argument('output', help='output image path (format inferred from extension, e.g. .svg, .png)')
    return p.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)
    try:
        render(args.input, args.output)
    except RenderError as e:
        print('render_graph.py: error: %s' % e, file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())

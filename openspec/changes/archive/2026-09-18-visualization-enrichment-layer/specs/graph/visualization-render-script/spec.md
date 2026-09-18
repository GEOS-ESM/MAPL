## Purpose

Provides the standalone, non-Fortran command-line tool that turns a
`.dot` file produced by `graph/visualization-export` into a rendered
image, fulfilling REQ-VIZ-002's requirement that rendering be delegated
to an established external tool rather than performed by MAPL/Graph
itself.

## ADDED Requirements

### Requirement: Script renders a DOT file to an image via Graphviz
Given a path to a `.dot` file and an output image path, the script
SHALL invoke the system's installed Graphviz `dot` executable to
produce a rendered image at the requested output path, in the format
implied by the output path's file extension.

#### Scenario: Valid DOT file renders successfully
- **WHEN** the script is run with a well-formed `.dot` file and an
  output path ending in a supported image extension (e.g. `.svg` or
  `.png`)
- **THEN** an image file is created at the output path and the script
  exits with a success status

#### Scenario: Output format follows the output path's extension
- **WHEN** the same input `.dot` file is rendered once to a `.svg`
  output path and once to a `.png` output path
- **THEN** each run produces an image in the format matching its own
  output path's extension

### Requirement: Missing Graphviz installation fails clearly
If the `dot` executable cannot be located, the script SHALL exit with a
non-zero status and an error message that identifies the missing
`dot` dependency, rather than a raw/unhandled exception traceback.

#### Scenario: `dot` not on PATH
- **WHEN** the script is run in an environment where no `dot` executable
  is discoverable
- **THEN** the script exits non-zero and prints a message identifying
  that Graphviz's `dot` could not be found

### Requirement: Graphviz layout failure is reported, not swallowed
If the `dot` executable runs but reports an error (e.g. malformed DOT
input), the script SHALL surface that failure to the caller — exiting
non-zero and including `dot`'s own error output — rather than reporting
success or silently producing no output file.

#### Scenario: Malformed DOT input
- **WHEN** the script is run against a file that is not valid DOT syntax
- **THEN** the script exits non-zero, no output image file is left
  behind claiming success, and `dot`'s own error text is included in
  the script's output

### Requirement: Script has no dependency beyond the Python standard library
The script SHALL run using only the Python standard library plus the
external `dot` executable — it SHALL NOT require installing any
additional Python package.

#### Scenario: Script runs in a plain Python environment
- **WHEN** the script is run in a Python environment with no
  third-party packages installed, but with Graphviz's `dot` available
  on `PATH`
- **THEN** the script runs and renders successfully

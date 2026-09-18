#!/usr/bin/env bash
set -euo pipefail

# Runs MpiServer with 5 PETs, then AsyncInputServer with 5 model PETs,
# 1 reader captain, and 2 reader workers.
if [[ $# -ne 2 ]]; then
  echo "usage: $0 WORK_DIR BUILD_DIR" >&2
  exit 2
fi

work_dir=$(cd "$1" && pwd)
build_dir=$(cd "$2" && pwd)
cache_file="$build_dir/CMakeCache.txt"
geos_x="$build_dir/bin/GEOS.x"

if [[ ! -x "$geos_x" || ! -f "$cache_file" ]]; then
  echo "invalid build directory: $build_dir" >&2
  exit 1
fi

mpiexec_cmd=$(perl -ne 'print $1 if /^MPIEXEC_EXECUTABLE:FILEPATH=(.*)$/' "$cache_file")
udunits_xml=$(perl -ne 'print $1 if /^udunits_XML_PATH:FILEPATH=(.*)$/' "$cache_file")
component_lib_dir="$build_dir/gridcomps/componentDriverGridComp"
export LD_LIBRARY_PATH="$build_dir/lib:$component_lib_dir:$build_dir/tests/MAPL3G_Component_Testing_Framework/gridcomps:${LD_LIBRARY_PATH:-}"
export DYLD_LIBRARY_PATH="$build_dir/lib:$component_lib_dir:$build_dir/tests/MAPL3G_Component_Testing_Framework/gridcomps:${DYLD_LIBRARY_PATH:-}"
export UDUNITS2_XML_PATH="$udunits_xml"
export ESMF_RUNTIME_COMPLIANCECHECK=OFF
export MAPL_ASYNC_INPUT_CACHE_SLOTS="${MAPL_ASYNC_INPUT_CACHE_SLOTS:-4}"

mpiexec_flags=()
if [[ "$(uname -s)" == Darwin ]]; then
  mpiexec_flags=(--use-hwthread-cpu --bind-to none -oversubscribe)
  component_dylib="$component_lib_dir/libMAPL.componentDriverGridComp.dylib"
  perl -0pi -e "s|sharedObj: libMAPL\.componentDriverGridComp|sharedObj: $component_dylib|g" \
    "$work_dir/GCM1.yaml" "$work_dir/GCM2.yaml"
fi

if [[ ! -f "$work_dir/test_20040416_2000.nc4" ]]; then
  echo "Generating benchmark NetCDF files"
  (
    cd "$work_dir"
    "$mpiexec_cmd" -n 5 "${mpiexec_flags[@]}" "$geos_x" cap1.yaml > prepare.log 2>&1
  )
fi

# cap1 advances this restart while generating the input sequence.
perl -0pi -e 's/currTime: .*/currTime: 2004-04-16T20:15:00/' "$work_dir/cap_restart2.yaml"

run_case() {
  local label=$1
  local np=$2
  local yaml=$3

  (
    cd "$work_dir"
    /usr/bin/time -p -o "$label.time" "$mpiexec_cmd" -n "$np" \
      "${mpiexec_flags[@]}" \
      "$geos_x" "$yaml" > "$label.log" 2>&1
  )

  echo "$label ($np PETs)"
  echo "mode: real-io"
  perl -ne 'print if /^real /' "$work_dir/$label.time"
  perl -ne 'print if /EXTDATA\.profile: EXTDATA/' "$work_dir/$label.log"
  perl -ne 'print if /AsyncInputServer (?:captain )?cache:/' "$work_dir/$label.log"
}

# The comparison consists of exactly these two timed launches.
run_case mpi 5 cap-mpi.yaml
run_case async 8 cap-async.yaml

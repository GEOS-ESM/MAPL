#!/usr/bin/env bash
set -euo pipefail

# Usage: run_async_perf_cases.sh WORK_ROOT BUILD_DIR [MPIEXEC]
#
# The overlap demo uses only real reader I/O. Model-side work is controlled by
# prepare_async_perf_cases.sh --model-delay; no artificial reader sleep is used.

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "usage: $0 WORK_ROOT BUILD_DIR [MPIEXEC]" >&2
  exit 2
fi

work_root=$1
build_dir=$2
mpiexec_cmd=${3:-}

mpi_dir="$work_root/perf-mpi8"
async_dir="$work_root/perf-async9"
geos_x="$build_dir/bin/GEOS.x"
cache_file="$build_dir/CMakeCache.txt"

if [[ -z "$mpiexec_cmd" ]]; then
  mpiexec_cmd=$(grep '^MPIEXEC_EXECUTABLE:FILEPATH=' "$cache_file" | cut -d= -f2-)
fi

if [[ ! -x "$geos_x" ]]; then
  echo "missing executable: $geos_x" >&2
  exit 1
fi

if [[ ! -x "$mpiexec_cmd" ]]; then
  echo "missing mpiexec: $mpiexec_cmd" >&2
  exit 1
fi

udunits_xml=$(grep '^udunits_XML_PATH:FILEPATH=' "$cache_file" | cut -d= -f2-)
if [[ -n "$udunits_xml" ]]; then
  export UDUNITS2_XML_PATH="$udunits_xml"
fi

export LD_LIBRARY_PATH="$build_dir/lib:$build_dir/gridcomps/componentDriverGridComp:$build_dir/tests/MAPL3G_Component_Testing_Framework/gridcomps:${LD_LIBRARY_PATH:-}"
export DYLD_LIBRARY_PATH="$build_dir/lib:$build_dir/gridcomps/componentDriverGridComp:$build_dir/tests/MAPL3G_Component_Testing_Framework/gridcomps:${DYLD_LIBRARY_PATH:-}"

reader_sleep="${MAPL_PERF_READER_SLEEP_SEC:-0}"
echo "reader_sleep (both servers): ${reader_sleep}s"

mpiexec_flags=( -oversubscribe )
if [[ "$(uname -s)" == "Darwin" ]]; then
  mpiexec_flags=( --use-hwthread-cpu "${mpiexec_flags[@]}" )
fi

echo "model_delay: $(grep 'model_delay' "$async_dir/cap2.yaml" 2>/dev/null | head -1 || echo 'not set')"
echo

run_case() {
  local np=$1
  local dir=$2
  local label=$3
  ( cd "$dir" && "$mpiexec_cmd" -n 8 "${mpiexec_flags[@]}" "$geos_x" cap1.yaml > cap1.log 2>&1 )
  ( cd "$dir" && env MAPL_PERF_READER_SLEEP_SEC="$reader_sleep" /usr/bin/time -p -o cap2.time "$mpiexec_cmd" -n "$np" "${mpiexec_flags[@]}" "$geos_x" cap2.yaml > cap2.log 2>&1 )

  echo "[$label] wall time"
  cat "$dir/cap2.time"
  echo "[$label] EXTDATA run profile"
  grep 'EXTDATA.profile: --run' "$dir/cap2.log" || true
  echo "[$label] model_delay (cap)"
  grep 'Cap model sleep\|model_delay' "$dir/cap2.log" | head -1 || echo '(no model delay)'
  echo "[$label] cache summary"
  grep 'AsyncInputServer cache:' "$dir/cap2.log" || echo '(no async cache summary for baseline)'
  echo
}

run_case 8 "$mpi_dir" "mpi8"
run_case 9 "$async_dir" "async9"

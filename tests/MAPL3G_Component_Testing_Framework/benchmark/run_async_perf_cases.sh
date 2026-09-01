#!/usr/bin/env bash
set -euo pipefail

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

run_case() {
  local np=$1
  local dir=$2
  local label=$3

  ( cd "$dir" && "$mpiexec_cmd" -n 8 -oversubscribe "$geos_x" cap1.yaml > cap1.log 2>&1 )
  ( cd "$dir" && /usr/bin/time -p -o cap2.time "$mpiexec_cmd" -n "$np" -oversubscribe "$geos_x" cap2.yaml > cap2.log 2>&1 )

  echo
  echo "[$label] wall time"
  cat "$dir/cap2.time"
  echo "[$label] EXTDATA run profile"
  grep 'EXTDATA.profile: --run' "$dir/cap2.log" || true
  echo "[$label] cache summary"
  grep 'AsyncInputServer cache:' "$dir/cap2.log" || echo '(no async cache summary for baseline)'
}

run_case 8 "$mpi_dir" "mpi8"
run_case 9 "$async_dir" "async9"

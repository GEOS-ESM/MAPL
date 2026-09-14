#!/usr/bin/env bash
set -euo pipefail

# Usage: run_async_perf_cases.sh WORK_ROOT BUILD_DIR [MPIEXEC]
#        [--model-pets N] [--reader-pets N[,N...]] [--repeats N]
#        [--placement hwthread|unbound] [--skip-prepare]
#
# The overlap demo uses only real reader I/O. Model-side work is controlled by
# prepare_async_perf_cases.sh --model-delay; no artificial reader sleep is used.

if [[ $# -lt 2 ]]; then
  echo "usage: $0 WORK_ROOT BUILD_DIR [MPIEXEC] [--model-pets N] [--reader-pets LIST] [--repeats N] [--placement MODE] [--skip-prepare]" >&2
  exit 2
fi

work_root=$1
build_dir=$2
shift 2
mpiexec_cmd=""
model_pets=8
reader_pets_list="2,3,4"
repeats=3
placement=hwthread
skip_prepare=false

if [[ $# -gt 0 && "$1" != --* ]]; then
  mpiexec_cmd=$1
  shift
fi

while [[ $# -gt 0 ]]; do
  case "$1" in
    --model-pets)
      model_pets=$2; shift 2 ;;
    --reader-pets)
      reader_pets_list=$2; shift 2 ;;
    --repeats)
      repeats=$2; shift 2 ;;
    --placement)
      placement=$2; shift 2 ;;
    --skip-prepare)
      skip_prepare=true; shift ;;
    *)
      echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

mpi_dir="$work_root/perf-mpi"
async_dir="$work_root/perf-async"
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

component_lib_dir="$build_dir/gridcomps/componentDriverGridComp"
export LD_LIBRARY_PATH="$build_dir/lib:$component_lib_dir:$build_dir/tests/MAPL3G_Component_Testing_Framework/gridcomps:${LD_LIBRARY_PATH:-}"
export DYLD_LIBRARY_PATH="$build_dir/lib:$component_lib_dir:$build_dir/tests/MAPL3G_Component_Testing_Framework/gridcomps:${DYLD_LIBRARY_PATH:-}"
export ESMF_RUNTIME_COMPLIANCECHECK=OFF

if [[ "$(uname -s)" == "Darwin" ]]; then
  component_dylib="$component_lib_dir/libMAPL.componentDriverGridComp.dylib"
  for dir in "$mpi_dir" "$async_dir"; do
    perl -0pi -e "s|sharedObj: libMAPL\.componentDriverGridComp|sharedObj: $component_dylib|g" \
      "$dir/GCM1.yaml" "$dir/GCM2.yaml"
  done
fi

reader_sleep="${MAPL_PERF_READER_SLEEP_SEC:-0}"
echo "reader_sleep (both servers): ${reader_sleep}s"

mpiexec_flags=( -oversubscribe )
if [[ "$(uname -s)" == "Darwin" ]]; then
  mpiexec_flags=( -x DYLD_LIBRARY_PATH -x ESMF_RUNTIME_COMPLIANCECHECK "${mpiexec_flags[@]}" )
  case "$placement" in
    hwthread)
      mpiexec_flags=( --use-hwthread-cpu --bind-to none "${mpiexec_flags[@]}" ) ;;
    unbound)
      mpiexec_flags=( --bind-to none "${mpiexec_flags[@]}" ) ;;
    *)
      echo "unknown placement: $placement" >&2; exit 2 ;;
  esac
fi

echo "model_delay: $(grep 'model_delay' "$async_dir/cap2.yaml" 2>/dev/null | head -1 || echo 'not set')"
echo "placement: $placement"
echo

prepare_files() {
  local dir=$1
  ( cd "$dir" && "$mpiexec_cmd" -n "$model_pets" "${mpiexec_flags[@]}" "$geos_x" cap1.yaml > cap1.log 2>&1 )
}

run_case() {
  local np=$1
  local dir=$2
  local label=$3
  local repeat=$4
  local log="cap2-${label}-r${repeat}.log"
  local timing="cap2-${label}-r${repeat}.time"
  ( cd "$dir" && env MAPL_PERF_READER_SLEEP_SEC="$reader_sleep" /usr/bin/time -p -o "$timing" "$mpiexec_cmd" -n "$np" "${mpiexec_flags[@]}" "$geos_x" cap2.yaml > "$log" 2>&1 )

  echo "[$label repeat=$repeat] wall time"
  cat "$dir/$timing"
  echo "[$label] EXTDATA profile"
  grep 'EXTDATA.profile: EXTDATA' "$dir/$log" || true
  echo "[$label] model_delay (cap)"
  grep 'Cap model sleep\|model_delay' "$dir/$log" | head -1 || echo '(no model delay)'
  echo "[$label] cache summary"
  grep 'AsyncInputServer cache:\|AsyncInputServer captain cache:' "$dir/$log" || echo '(no async cache summary for baseline)'
  echo
}

if ! $skip_prepare; then
  prepare_files "$mpi_dir"
  prepare_files "$async_dir"
fi

IFS=',' read -r -a reader_counts <<< "$reader_pets_list"
for ((repeat=1; repeat<=repeats; repeat++)); do
  run_case "$model_pets" "$mpi_dir" "mpi${model_pets}" "$repeat"
  for reader_pets in "${reader_counts[@]}"; do
    if (( reader_pets < 2 )); then
      echo "reader-pets must include one captain and at least one worker" >&2
      exit 2
    fi
    total_pets=$((model_pets + reader_pets))
    run_case "$total_pets" "$async_dir" "async${total_pets}-r${reader_pets}" "$repeat"
  done
done

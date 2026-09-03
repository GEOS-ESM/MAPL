#!/usr/bin/env bash
set -euo pipefail

# Usage: prepare_async_perf_cases.sh WORK_ROOT [--model-delay SECS] [--grid IMxJM]
#
# --model-delay SECS  Insert a cap-level model_delay (seconds) into both
#                     cap2.yaml variants.  Use with ASYNC_READER_SLEEP_SEC
#                     to demonstrate overlap:
#
#                       bash prepare_async_perf_cases.sh /tmp/p \
#                            --model-delay 0.5 --grid 512x384
#                       ASYNC_READER_SLEEP_SEC=0.3 \
#                         bash run_async_perf_cases.sh /tmp/p build
#
# --grid IMxJM        Override the lat-lon grid size in all YAML configs.
#                     Default: 13x9 (tiny test grid from case49).
#                     A larger grid (e.g. 512x384) produces bigger NC4 files
#                     so that real file-read time becomes significant and the
#                     async9 cache-hit benefit is measurable without needing
#                     artificial reader sleep.
#
# Overlap logic:
#   model_delay(SECS) > ASYNC_READER_SLEEP_SEC or real read time
#   → async9 reader finishes next-prefetch inside the model window
#   → next timestep's current read is a free cache hit
#   → mpi8 must pay (read time + model_delay) per step; async9 pays only model_delay

model_delay_val=""
grid_im=13
grid_jm=9

while [[ $# -gt 0 ]]; do
  case "$1" in
    --model-delay)
      model_delay_val="$2"; shift 2 ;;
    --grid)
      grid_im="${2%%x*}"; grid_jm="${2##*x}"; shift 2 ;;
    *)
      work_root="$1"; shift ;;
  esac
done

if [[ -z "${work_root:-}" ]]; then
  echo "usage: $0 WORK_ROOT [--model-delay SECS] [--grid IMxJM]" >&2
  exit 2
fi

script_dir=$(cd "$(dirname "$0")" && pwd)
base_case_dir="$script_dir/../test_cases/case49"

mpi_dir="$work_root/perf-mpi8"
async_dir="$work_root/perf-async9"

rm -rf "$mpi_dir" "$async_dir"
mkdir -p "$mpi_dir" "$async_dir"
cp -R "$base_case_dir/." "$mpi_dir/"
cp -R "$base_case_dir/." "$async_dir/"

cap1_run_times=$(cat <<'EOF'
    - '2004-04-15T21:00:00'
    - '2004-04-16T21:00:00'
    - '2004-04-17T21:00:00'
    - '2004-04-18T21:00:00'
    - '2004-04-19T21:00:00'
    - '2004-04-20T21:00:00'
    - '2004-04-21T21:00:00'
    - '2004-04-22T21:00:00'
    - '2004-04-23T21:00:00'
    - '2004-04-24T21:00:00'
    - '2004-04-25T21:00:00'
    - '2004-04-26T21:00:00'
    - '2004-04-27T21:00:00'
    - '2004-04-28T21:00:00'
    - '2004-04-29T21:00:00'
    - '2004-04-30T21:00:00'
    - '2004-05-01T21:00:00'
    - '2004-05-02T21:00:00'
    - '2004-05-03T21:00:00'
    - '2004-05-04T21:00:00'
    - '2004-05-05T21:00:00'
    - '2004-05-06T21:00:00'
    - '2004-05-07T21:00:00'
    - '2004-05-08T21:00:00'
EOF
)

cap2_run_times=$(cat <<'EOF'
    - '2004-04-20T09:00:00'
    - '2004-04-20T21:00:00'
    - '2004-04-21T09:00:00'
    - '2004-04-21T21:00:00'
    - '2004-04-22T09:00:00'
    - '2004-04-22T21:00:00'
    - '2004-04-23T09:00:00'
    - '2004-04-23T21:00:00'
    - '2004-04-24T09:00:00'
    - '2004-04-24T21:00:00'
    - '2004-04-25T09:00:00'
    - '2004-04-25T21:00:00'
    - '2004-04-26T09:00:00'
    - '2004-04-26T21:00:00'
    - '2004-04-27T09:00:00'
    - '2004-04-27T21:00:00'
    - '2004-04-28T09:00:00'
    - '2004-04-28T21:00:00'
    - '2004-04-29T09:00:00'
    - '2004-04-29T21:00:00'
    - '2004-04-30T09:00:00'
    - '2004-04-30T21:00:00'
    - '2004-05-01T09:00:00'
    - '2004-05-01T21:00:00'
    - '2004-05-02T09:00:00'
    - '2004-05-02T21:00:00'
    - '2004-05-03T09:00:00'
    - '2004-05-03T21:00:00'
    - '2004-05-04T09:00:00'
    - '2004-05-04T21:00:00'
    - '2004-05-05T09:00:00'
    - '2004-05-05T21:00:00'
    - '2004-05-06T09:00:00'
    - '2004-05-06T21:00:00'
    - '2004-05-07T09:00:00'
    - '2004-05-07T21:00:00'
EOF
)

cat > "$mpi_dir/cap1.yaml" <<EOF
esmf:
  logKindFlag: ESMF_LOGKIND_MULTI
  logAppendFlag: false

mapl:
  model_petcount: 8
  pflogger_cfg_file: logging.yaml

cap:
  name: cap
  skip_restart_write: true
  restart: cap_restart1.yaml

  clock:
    dt: PT15M
    start: 2004-04-14T21:00:00
    stop: 2999-03-02T21:00:00
    segment_duration: P25D

  extdata_name: EXTDATA
  history_name: HIST
  root_name: GCM

  run_extdata: true
  run_history: true

  run_times:
$cap1_run_times

  mapl:
    setServices:
      sharedObj: libMAPL.cap
    children:
      GCM:
        config_file: GCM1.yaml
      EXTDATA:
        config_file: extdata1.yaml
      HIST:
        config_file: history1.yaml
EOF

cp "$mpi_dir/cap1.yaml" "$async_dir/cap1.yaml"

cat > "$mpi_dir/cap2.yaml" <<EOF
esmf:
  logKindFlag: ESMF_LOGKIND_MULTI
  logAppendFlag: false

mapl:
  model_petcount: 8
  pflogger_cfg_file: logging.yaml

cap:
  name: cap
  skip_restart_write: true
  restart: cap_restart2.yaml

  clock:
    dt: PT15M
    start: 2004-01-20T00:00:00
    stop: 2999-03-02T21:00:00
    segment_duration: P30D

  extdata_name: EXTDATA
  history_name: HIST
  root_name: GCM

  run_extdata: true
  run_history: true

  run_times:
$cap2_run_times

  mapl:
    setServices:
      sharedObj: libMAPL.cap
    children:
      GCM:
        config_file: GCM2.yaml
      EXTDATA:
        config_file: extdata2.yaml
      HIST:
        config_file: history2.yaml
EOF

cat > "$async_dir/cap2.yaml" <<EOF
esmf:
  logKindFlag: ESMF_LOGKIND_MULTI
  logAppendFlag: false

mapl:
  model_petcount: 8
  pflogger_cfg_file: logging.yaml
  servers:
    async_input_server:
      local: true
      subclass: AsyncInputServer

cap:
  name: cap
  skip_restart_write: true
  restart: cap_restart2.yaml

  clock:
    dt: PT15M
    start: 2004-01-20T00:00:00
    stop: 2999-03-02T21:00:00
    segment_duration: P30D

  extdata_name: EXTDATA
  history_name: HIST
  root_name: GCM

  run_extdata: true
  run_history: true

  run_times:
$cap2_run_times

  mapl:
    setServices:
      sharedObj: libMAPL.cap
    children:
      GCM:
        config_file: GCM2.yaml
      EXTDATA:
        config_file: extdata2.yaml
      HIST:
        config_file: history2.yaml
EOF

perl -0pi -e "s/^input_server_name: async_input_server\n//m" "$mpi_dir/extdata2.yaml"
perl -0pi -e "s/^log_files_read: .*\n//m" "$mpi_dir/extdata1.yaml" "$mpi_dir/extdata2.yaml" "$async_dir/extdata1.yaml" "$async_dir/extdata2.yaml"

# Switch GCM2 run mode to FillImports (no-op) for both benchmark cases
# (we are measuring performance, not validating field values here).
perl -0pi -e "s/^RUN_MODE:.*$/RUN_MODE: FillImports/m" \
  "$mpi_dir/GCM2.yaml" "$async_dir/GCM2.yaml"

# Inject grid size into all YAML files that reference im_world / jm_world.
# This controls the size of the written NC4 files and thus real read time.
if [[ "$grid_im" != "13" || "$grid_jm" != "9" ]]; then
  for dir in "$mpi_dir" "$async_dir"; do
    for yaml in "$dir/GCM1.yaml" "$dir/GCM2.yaml" "$dir/history1.yaml" "$dir/history2.yaml"; do
      perl -0pi -e "s/im_world: \d+/im_world: ${grid_im}/" "$yaml"
      perl -0pi -e "s/jm_world: \d+/jm_world: ${grid_jm}/" "$yaml"
    done
  done
  echo "grid size set to ${grid_im}x${grid_jm} in GCM1/GCM2/history1/history2 yaml files"
fi

# Inject optional model_delay into both cap2 variants.
if [[ -n "$model_delay_val" ]]; then
  for dir in "$mpi_dir" "$async_dir"; do
    # Insert "  model_delay: <val>" after the "cap:" section header line.
    perl -0pi -e "s/(^cap:\n)(  name:)/\${1}  model_delay: ${model_delay_val}\n\${2}/m" \
      "$dir/cap2.yaml"
  done
  echo "model_delay set to ${model_delay_val}s in both cap2.yaml files"
fi

cat <<EOF
Prepared:
  $mpi_dir
  $async_dir

Next:
  bash tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh $work_root /path/to/build

Overlap demo (reader_sleep=0.3s, model_delay=${model_delay_val:-not set}s):
  bash tests/MAPL3G_Component_Testing_Framework/benchmark/prepare_async_perf_cases.sh $work_root --model-delay 0.5
  ASYNC_READER_SLEEP_SEC=0.3 bash tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh $work_root /path/to/build
EOF

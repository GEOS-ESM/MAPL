#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 WORK_ROOT" >&2
  exit 2
fi

work_root=$1
script_dir=$(cd "$(dirname "$0")" && pwd)
base_case_dir="$script_dir/../test_cases/case49"

mpi_dir="$work_root/perf-mpi8"
async_dir="$work_root/perf-async9"

rm -rf "$mpi_dir" "$async_dir"
mkdir -p "$mpi_dir" "$async_dir"
cp -R "$base_case_dir/." "$mpi_dir/"
cp -R "$base_case_dir/." "$async_dir/"

run_times=$(cat <<'EOF'
    - '2004-04-16T09:00:00'
    - '2004-04-16T09:15:00'
    - '2004-04-16T09:30:00'
    - '2004-04-16T09:45:00'
    - '2004-04-16T10:00:00'
    - '2004-04-16T10:15:00'
    - '2004-04-16T10:30:00'
    - '2004-04-16T10:45:00'
    - '2004-04-16T11:00:00'
    - '2004-04-16T11:15:00'
    - '2004-04-16T11:30:00'
    - '2004-04-16T11:45:00'
    - '2004-04-16T12:00:00'
    - '2004-04-16T12:15:00'
    - '2004-04-16T12:30:00'
    - '2004-04-16T12:45:00'
    - '2004-04-16T13:00:00'
    - '2004-04-16T13:15:00'
    - '2004-04-16T13:30:00'
    - '2004-04-16T13:45:00'
    - '2004-04-16T14:00:00'
    - '2004-04-16T14:15:00'
    - '2004-04-16T14:30:00'
    - '2004-04-16T14:45:00'
    - '2004-04-16T15:00:00'
    - '2004-04-16T15:15:00'
    - '2004-04-16T15:30:00'
    - '2004-04-16T15:45:00'
    - '2004-04-16T16:00:00'
    - '2004-04-16T16:15:00'
    - '2004-04-16T16:30:00'
    - '2004-04-16T16:45:00'
    - '2004-04-16T17:00:00'
    - '2004-04-16T17:15:00'
    - '2004-04-16T17:30:00'
    - '2004-04-16T17:45:00'
    - '2004-04-16T18:00:00'
    - '2004-04-16T18:15:00'
    - '2004-04-16T18:30:00'
    - '2004-04-16T18:45:00'
    - '2004-04-16T19:00:00'
    - '2004-04-16T19:15:00'
    - '2004-04-16T19:30:00'
    - '2004-04-16T19:45:00'
    - '2004-04-16T20:00:00'
    - '2004-04-16T20:15:00'
    - '2004-04-16T20:30:00'
    - '2004-04-16T20:45:00'
    - '2004-04-16T21:00:00'
    - '2004-04-16T21:15:00'
    - '2004-04-16T21:30:00'
    - '2004-04-16T21:45:00'
    - '2004-04-16T22:00:00'
    - '2004-04-16T22:15:00'
    - '2004-04-16T22:30:00'
    - '2004-04-16T22:45:00'
    - '2004-04-16T23:00:00'
    - '2004-04-16T23:15:00'
    - '2004-04-16T23:30:00'
    - '2004-04-16T23:45:00'
EOF
)

cat > "$mpi_dir/cap1.yaml" <<'EOF'
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
    segment_duration: P6D

  extdata_name: EXTDATA
  history_name: HIST
  root_name: GCM

  run_extdata: true
  run_history: true

  run_times:
    - '2004-04-15T21:00:00'
    - '2004-04-16T21:00:00'
    - '2004-04-17T21:00:00'
    - '2004-04-18T21:00:00'

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
    segment_duration: P1D

  extdata_name: EXTDATA
  history_name: HIST
  root_name: GCM

  run_extdata: true
  run_history: true

  run_times:
$run_times

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
    segment_duration: P1D

  extdata_name: EXTDATA
  history_name: HIST
  root_name: GCM

  run_extdata: true
  run_history: true

  run_times:
$run_times

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

cat <<EOF
Prepared:
  $mpi_dir
  $async_dir

Next:
  bash tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh $work_root /path/to/build
EOF

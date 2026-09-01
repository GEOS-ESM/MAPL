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
    segment_duration: P1D

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
    segment_duration: P1D

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

cat <<EOF
Prepared:
  $mpi_dir
  $async_dir

Next:
  bash tests/MAPL3G_Component_Testing_Framework/benchmark/run_async_perf_cases.sh $work_root /path/to/build
EOF

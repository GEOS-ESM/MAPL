#!/usr/bin/env bash
set -euo pipefail

# Run once to create the shared 512x384 input data and the two benchmark YAMLs.
if [[ $# -ne 1 ]]; then
  echo "usage: $0 WORK_DIR" >&2
  exit 2
fi

work_dir=$1
script_dir=$(cd "$(dirname "$0")" && pwd)
case_dir="$script_dir/../test_cases/pfio/case05"

mkdir -p "$work_dir"
cp -R "$case_dir/." "$work_dir/"
cp "$script_dir/cap-mpi.yaml" "$script_dir/cap-async.yaml" "$work_dir/"
cp "$work_dir/extdata2.yaml" "$work_dir/extdata-async.yaml"
cp "$work_dir/extdata2.yaml" "$work_dir/extdata-mpi.yaml"

perl -0pi -e 's/^input_server_name: async_input_server\n//m' "$work_dir/extdata-mpi.yaml"
perl -0pi -e 's/^log_files_read: .*\n//m' "$work_dir/extdata-mpi.yaml" "$work_dir/extdata-async.yaml"
perl -0pi -e 's/^RUN_MODE:.*$/RUN_MODE: FillImports/m' "$work_dir/GCM2.yaml"
perl -0pi -e 's/model_petcount: 1/model_petcount: 5/; s/start: 2004-04-14T21:00:00/start: 2004-04-16T19:45:00/; s/segment_duration: P6D/segment_duration: PT5H/; s/\n  run_times:\n(?:    - .*\n)+/\n/' "$work_dir/cap1.yaml"
perl -0pi -e 's/currTime: .*/currTime: 2004-04-16T19:45:00/' "$work_dir/cap_restart1.yaml"
perl -0pi -e 's/im_world: 13/im_world: 512/g; s/jm_world: 9/jm_world: 384/g' \
  "$work_dir/GCM1.yaml" "$work_dir/GCM2.yaml" "$work_dir/history1.yaml"
perl -0pi -e 's/run_next_step: false/run_next_step: true/; s/frequency: PT1H/frequency: PT15M/g; s/%c_%y4%m2%d2\.nc4/%c_%y4%m2%d2_%h2%n2.nc4/g' \
  "$work_dir/history1.yaml"
perl -0pi -e 's/template: "test_%y4%m2%d2\.nc4"/template: "test_%y4%m2%d2_%h2%n2.nc4"\n      ref_time: "2004-04-16T20:00:00"\n      freq: PT15M/; s/template: "test_b_%y4%m2%d2\.nc4"/template: "test_b_%y4%m2%d2_%h2%n2.nc4"\n      ref_time: "2004-04-16T20:00:00"\n      freq: PT15M/; s/time_interpolation: true/time_interpolation: false/' \
  "$work_dir/extdata-mpi.yaml" "$work_dir/extdata-async.yaml"

echo "Prepared benchmark input in $work_dir"

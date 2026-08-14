#!/usr/bin/env python3
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
GENERATOR = ROOT / "cmake" / "generate_error_codes.py"
CATALOG = ROOT / "utils" / "Constants" / "mapl_error_codes.yaml"
FIXTURES = Path(__file__).resolve().parent


def run_generator(source, output):
    return subprocess.run(
        [sys.executable, str(GENERATOR), str(source), str(output)],
        capture_output=True,
        text=True,
    )


def main():
    with tempfile.TemporaryDirectory() as directory:
        output = Path(directory) / "MAPL_ErrorCodes_generated.F90"
        result = run_generator(CATALOG, output)
        if result.returncode != 0 or "MAPL_MISSING_FILE" not in output.read_text():
            raise AssertionError(result.stderr or "valid catalog generation failed")

        for fixture in (
            "mapl_error_codes_malformed.yaml",
            "mapl_error_codes_partial.yaml",
            "mapl_error_codes_duplicate.yaml",
            "mapl_error_codes_empty.yaml",
            "mapl_error_codes_v2.yaml",
        ):
            result = run_generator(FIXTURES / fixture, output)
            if result.returncode == 0:
                raise AssertionError(f"invalid fixture accepted: {fixture}")


if __name__ == "__main__":
    main()

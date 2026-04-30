"""
Test that Python constants in MAPL/constants.py are consistent with the
Fortran constants defined in shared/Constants/*.F90.

The test parses the Fortran source files directly so that any change to a
Fortran constant will immediately cause this test to fail, prompting the
developer to update constants.py as well.

The lists of shared vs. Fortran-only constants are maintained in
constants_registry.yaml (same directory as this file) rather than being
hard-coded here.
"""

import math
import re
from pathlib import Path

import pytest
import yaml

# ---------------------------------------------------------------------------
# Locate the repo root relative to this test file:
#   Python/MAPL/tests/test_constants_consistency.py  ->  repo root
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parents[3]
FORTRAN_CONSTANTS_DIR = REPO_ROOT / "shared" / "Constants"
REGISTRY_PATH = Path(__file__).parent / "constants_registry.yaml"

FORTRAN_FILES = [
    "MathConstants.F90",
    "PhysicalConstants.F90",
    "EarthConstants.F90",
    "EarthAtmosphericConstants.F90",
    "InternalConstants.F90",
]


# ---------------------------------------------------------------------------
# Load registry
# ---------------------------------------------------------------------------

def _load_registry():
    """
    Load constants_registry.yaml and return three structures:

    shared_numeric  : dict  python_name -> fortran_name
    shared_string   : dict  python_name -> fortran_name
    fortran_only    : set   of Fortran constant names
    """
    with REGISTRY_PATH.open() as fh:
        reg = yaml.safe_load(fh)

    def _to_dict(entries):
        """Convert a list of bare names or {python: fortran} mappings to a dict."""
        result = {}
        for entry in entries or []:
            if isinstance(entry, str):
                result[entry] = entry
            elif isinstance(entry, dict):
                # Bare name stored as dict by YAML loader when it's just a string key?
                # Actually entries like "python_name: fortran_name" would be single-key dicts.
                for k, v in entry.items():
                    result[k] = v
        return result

    shared_numeric = _to_dict(reg.get("shared_numeric", []))
    shared_string = _to_dict(reg.get("shared_string", []))

    fortran_only = set()
    for entry in reg.get("fortran_only", []):
        if isinstance(entry, str):
            fortran_only.add(entry)
        elif isinstance(entry, dict):
            name = entry.get("name")
            if name:
                fortran_only.add(name)

    return shared_numeric, shared_string, fortran_only


SHARED_NUMERIC_CONSTANTS, SHARED_STRING_CONSTANTS, FORTRAN_ONLY_CONSTANTS = _load_registry()

# Convenience: the full set of Fortran names already covered by the
# consistency tests (no need to flag these in the coverage check)
_COVERED_FORTRAN_NAMES: set = (
    set(SHARED_NUMERIC_CONSTANTS.values()) |
    set(SHARED_STRING_CONSTANTS.values())
)

#: Relative tolerance for floating-point comparisons (single-precision ~1e-6)
REL_TOL = 1e-5

#: Absolute tolerance for values that are very close to zero
ABS_TOL = 1e-30


# ---------------------------------------------------------------------------
# Fortran source parser
# ---------------------------------------------------------------------------

def _strip_preprocessor_else_blocks(text: str) -> str:
    """
    Remove the #if defined(CODATA_2018_CONSTANTS) ... #else ... #endif
    preprocessor blocks, keeping only the #else branch (i.e. the default
    build without CODATA_2018 constants).  Any other #if / #endif pairs
    are left untouched.
    """
    pattern = re.compile(
        r"#if\s+defined\(CODATA_2018_CONSTANTS\).*?#else(.*?)#endif",
        re.DOTALL,
    )
    return pattern.sub(lambda m: m.group(1), text)


def _join_continuation_lines(text: str) -> str:
    """Join Fortran continuation lines (trailing &) into a single line."""
    return re.sub(r"&\s*\n\s*", " ", text)


def parse_fortran_constants(use_codata_2018: bool = False) -> dict:
    """
    Parse all numeric/string parameters from the Fortran Constants source
    files and return them as a plain Python dict {name: value}.

    Derived parameters (those expressed as arithmetic on other parameters)
    are evaluated in source order so forward references within a file work
    correctly.  Each enum, bind(c) block resets its own auto-increment
    counter independently.

    Only the default (non-CODATA-2018) values are used unless
    *use_codata_2018* is True.
    """
    constants: dict = {}

    def _eval_rhs(rhs: str, enum_counter_box: list) -> object:
        """
        Evaluate a Fortran parameter RHS expression in the context of
        already-collected *constants*.

        *enum_counter_box* is a one-element list used as a mutable cell for
        the current enum block's auto-increment value.  Pass None when not
        inside an enum block.
        """
        if rhs is None:
            # Auto-increment enumerator
            val = enum_counter_box[0]
            enum_counter_box[0] += 1
            return val

        rhs = rhs.strip()

        # String literals
        if rhs.startswith(("'", '"')):
            return rhs.strip("'\"")

        rhs_py = rhs

        # Replace Fortran intrinsics with Python equivalents BEFORE touching
        # identifier names so we don't accidentally mangle constant names.
        rhs_py = re.sub(r"\bselected_real_kind\s*\([^)]+\)", "8", rhs_py, flags=re.IGNORECASE)
        rhs_py = re.sub(r"\bselected_int_kind\s*\([^)]+\)", "8", rhs_py, flags=re.IGNORECASE)
        rhs_py = re.sub(r"\bkind\s*\([^)]+\)", "8", rhs_py, flags=re.IGNORECASE)

        # huge() / -huge() — approximations for REAL32 and REAL64
        rhs_py = re.sub(r"-\s*huge\s*\(1\._?REAL64[^)]*\)", "(-1.7976931348623157e+308)", rhs_py, flags=re.IGNORECASE)
        rhs_py = re.sub(r"-\s*huge\s*\([^)]*\)", "(-3.4028235e+38)", rhs_py, flags=re.IGNORECASE)
        rhs_py = re.sub(r"huge\s*\([^)]*\)", "3.4028235e+38", rhs_py, flags=re.IGNORECASE)

        # Replace already-known constant names with their Python values.
        # Sort longest-first to avoid partial name substitutions.
        for k in sorted(constants.keys(), key=len, reverse=True):
            v = constants[k]
            if isinstance(v, (int, float)):
                rhs_py = re.sub(r"\b" + re.escape(k) + r"\b", repr(v), rhs_py)

        # NOW strip Fortran kind suffixes — only when attached to a numeric
        # literal (i.e., preceded by a digit or closing paren), not to names.
        rhs_py = re.sub(r"(?<=[0-9)])\s*_(?:REAL64|REAL32|R8|R4)\b", "", rhs_py)

        # Replace Fortran d/D exponent with e — handles: 3.14d0, 1.d0, 180.d0, 1D-5
        rhs_py = re.sub(r"(?<=[0-9.])[dD](?=[0-9+\-])", "e", rhs_py)

        # Clean up any remaining _KIND suffixes after digits
        rhs_py = re.sub(r"(\d)_\w+", r"\1", rhs_py)

        try:
            return eval(rhs_py, {"__builtins__": {}}, {"math": math})  # noqa: S307
        except Exception:
            return None  # unparseable — skip

    for fname in FORTRAN_FILES:
        fpath = FORTRAN_CONSTANTS_DIR / fname
        text = fpath.read_text()

        if not use_codata_2018:
            text = _strip_preprocessor_else_blocks(text)

        text = _join_continuation_lines(text)

        in_enum = False
        enum_counter_box = [0]  # mutable cell for current enum auto-increment

        for line in text.splitlines():
            # Strip inline comments
            line = line.split("!")[0].strip()

            # Detect enum block boundaries to manage per-block counters
            if re.match(r"enum\b", line, re.IGNORECASE):
                in_enum = True
                enum_counter_box = [0]  # reset counter for each new enum block
                continue
            if re.match(r"endenum\b", line, re.IGNORECASE):
                in_enum = False
                continue

            if in_enum:
                # enumerator :: NAME = VALUE   or   enumerator NAME
                m = re.match(r"enumerator\s*(?:::\s*)?(\w+)\s*(?:=\s*(.+))?", line, re.IGNORECASE)
                if m:
                    name = m.group(1).strip()
                    rhs = m.group(2).strip() if m.group(2) else None
                    if rhs is not None:
                        # Explicit value — evaluate and update the counter
                        val = _eval_rhs(rhs, enum_counter_box)
                        if val is not None:
                            enum_counter_box[0] = int(val) + 1
                            constants[name] = int(val)
                    else:
                        val = _eval_rhs(None, enum_counter_box)
                        constants[name] = int(val)
                continue

            # Match:  real[(kind=...)] [, parameter]* :: NAME = VALUE
            #         integer[(kind=...)] [, parameter]* :: NAME = VALUE
            #         character(len=*) [, parameter]* :: NAME = 'VALUE'
            m = re.match(
                r"(?:real|integer|character)[^:]*,\s*parameter\s*::\s*"
                r"(\w+)\s*=\s*(.+)",
                line,
                re.IGNORECASE,
            )
            if m:
                name, rhs = m.group(1).strip(), m.group(2).strip()
                val = _eval_rhs(rhs, None)
                if val is not None:
                    constants[name] = val

    return constants


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def fortran_constants() -> dict:
    return parse_fortran_constants(use_codata_2018=False)


@pytest.fixture(scope="module")
def python_constants() -> dict:
    """Return the MAPL Python constants as a plain dict."""
    import MAPL.constants as _c
    return {k: v for k, v in vars(_c).items() if not k.startswith("_")}


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

class TestNumericConsistency:
    """Each numeric constant present in both files must agree to REL_TOL."""

    @pytest.mark.parametrize("py_name,f_name", list(SHARED_NUMERIC_CONSTANTS.items()))
    def test_constant_matches(self, py_name, f_name, fortran_constants, python_constants):
        assert py_name in python_constants, (
            f"'{py_name}' is missing from Python constants.py"
        )
        assert f_name in fortran_constants, (
            f"'{f_name}' could not be parsed from the Fortran Constants sources"
        )

        py_val = python_constants[py_name]
        f_val = fortran_constants[f_name]

        assert isinstance(py_val, (int, float)), (
            f"Python '{py_name}' = {py_val!r} is not numeric"
        )
        assert isinstance(f_val, (int, float)), (
            f"Fortran '{f_name}' = {f_val!r} is not numeric"
        )

        if py_val == f_val:
            return  # exact match — no need for tolerance check

        assert math.isclose(float(py_val), float(f_val), rel_tol=REL_TOL, abs_tol=ABS_TOL), (
            f"Mismatch for {py_name}:\n"
            f"  Python  {py_name} = {py_val!r}\n"
            f"  Fortran {f_name} = {f_val!r}\n"
            f"  relative difference = {abs(py_val - f_val) / max(abs(f_val), 1e-300):.2e}"
        )


class TestStringConsistency:
    """String constants must match exactly."""

    @pytest.mark.parametrize("py_name,f_name", list(SHARED_STRING_CONSTANTS.items()))
    def test_string_constant_matches(self, py_name, f_name, fortran_constants, python_constants):
        assert py_name in python_constants, (
            f"'{py_name}' is missing from Python constants.py"
        )
        assert f_name in fortran_constants, (
            f"'{f_name}' could not be parsed from the Fortran Constants sources"
        )
        assert python_constants[py_name] == fortran_constants[f_name], (
            f"String mismatch for {py_name}:\n"
            f"  Python  = {python_constants[py_name]!r}\n"
            f"  Fortran = {fortran_constants[f_name]!r}"
        )


class TestKnownIssues:
    """
    Tests that verify previously-known inconsistencies have been resolved,
    and guard against regressions.
    """

    def test_mapl_undef_matches_fortran_sentinel(self, python_constants):
        """
        MAPL_UNDEF must match the Fortran -huge(1.) sentinel (~-3.4e38).
        Previously Python had 1.0e15 which was completely wrong.
        """
        py_undef = python_constants.get("MAPL_UNDEF")
        assert py_undef is not None, "MAPL_UNDEF is missing from Python constants.py"
        assert math.isclose(float(py_undef), -3.4028235e+38, rel_tol=1e-5), (
            f"MAPL_UNDEF = {py_undef!r} does not match Fortran -huge(1.) ~ -3.4028235e+38"
        )

    def test_mapl_cpl_notneeded_uses_canonical_name(self, python_constants):
        """
        The canonical Fortran name is MAPL_CplNOTNEEDED (no extra underscore).
        The old Python name MAPL_Cpl_NOTNEEDED has been removed.
        """
        assert "MAPL_CplNOTNEEDED" in python_constants, (
            "MAPL_CplNOTNEEDED is missing from Python constants.py"
        )
        assert "MAPL_Cpl_NOTNEEDED" not in python_constants, (
            "Old misspelled name MAPL_Cpl_NOTNEEDED should have been removed"
        )


class TestCoverage:
    """
    Ensure that every numeric/integer constant exported by the Fortran sources
    is either covered by the consistency tests OR explicitly listed in
    constants_registry.yaml under 'fortran_only'.

    If a new constant is added to a Fortran .F90 file and is not accounted for
    here, this test fails — prompting the developer to either add it to
    Python/MAPL/constants.py (and shared_numeric in the registry) or add it to
    the fortran_only section with a reason explaining why it is Fortran-only.
    """

    def test_no_unaccounted_fortran_constants(self, fortran_constants):
        unaccounted = []
        for name, value in fortran_constants.items():
            if not isinstance(value, (int, float)):
                continue  # string constants handled separately
            if name in _COVERED_FORTRAN_NAMES:
                continue
            if name in FORTRAN_ONLY_CONSTANTS:
                continue
            unaccounted.append(f"  {name} = {value!r}")

        assert not unaccounted, (
            "The following Fortran constants are not covered by the Python tests.\n"
            "For each one, either:\n"
            "  (a) add it to Python/MAPL/constants.py and shared_numeric in\n"
            "      Python/MAPL/tests/constants_registry.yaml, or\n"
            "  (b) add it to the fortran_only section of constants_registry.yaml\n"
            "      with a 'reason' field explaining why it is Fortran-only.\n\n"
            "Unaccounted constants:\n" + "\n".join(sorted(unaccounted))
        )

    def test_no_unaccounted_fortran_string_constants(self, fortran_constants):
        unaccounted = []
        for name, value in fortran_constants.items():
            if not isinstance(value, str):
                continue
            if name in _COVERED_FORTRAN_NAMES:
                continue
            if name in FORTRAN_ONLY_CONSTANTS:
                continue
            unaccounted.append(f"  {name} = {value!r}")

        assert not unaccounted, (
            "The following Fortran string constants are not covered by the Python tests.\n"
            "For each one, either:\n"
            "  (a) add it to Python/MAPL/constants.py and shared_string in\n"
            "      Python/MAPL/tests/constants_registry.yaml, or\n"
            "  (b) add it to the fortran_only section of constants_registry.yaml\n"
            "      with a 'reason' field explaining why it is Fortran-only.\n\n"
            "Unaccounted string constants:\n" + "\n".join(sorted(unaccounted))
        )

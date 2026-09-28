#!/usr/bin/env python3
import re
import argparse
import sys
from functools import reduce
from pathlib import Path
from itertools import filterfalse, chain, tee, takewhile, dropwhile
from collections import defaultdict
from os import linesep as newline
from operator import methodcaller
from io import StringIO

#================================= CONSTANTS ==================================#
SUCCESS = 0
FAILURE = SUCCESS-1
# Blank string
BLANK = r''
# Output record separator
RS = r' | '
# Line continuation character
AMP = r'&'

REQ_ARGS = ('GC', 'SHORT_NAME')
# These are used for ordering the output.
FIRST_COLUMNS = ('SHORT_NAME', 'UNITS', 'DIMS', 'VLOCATION')
LAST_COLUMNS = ('LONG_NAME')
# These are columns that should not be used as column headings.
IGNORE_COLUMNS = ('GC',)

#============================= REGULAR EXPRESSIONS ============================#
# This regular expression is used for process the state and arguments for MAPL_Add___Specs calls.
SUBROUTINE_PATTERN = re.compile(r'call\s+MAPL_Add(?P<state>\w+)Spec\s*\((?P<arguments>.*)\)\s*$', re.IGNORECASE)

# This regular expression is used to return the portion of a code line before any comment.
CODE_RE = r'\s*([^!]*)'

# These regular expressions are for future use to improve processing of arrays, function calls, and groups.
LINE_REGEXES = [r'(".*?"\s*)',
    r'(\'.*?\'\s*)',
    r'(?P<comment>!.*$)',
    r'(?P<delimiter>,\s*)',
    r'(?P<continuation>&\s*)',
    r'(?P<spaces>\s+)',
    r'(?P<open_array>\[\s*)',
    r'(?P<close_array>\]\s*)',
    r'(?P<open_group>\(\s*)',
    r'(?P<close_group>\)\s*)',
    r'([^\[\]()\'",!&\s]+\s*)']
LINE_REGEX_DICT = {'quote': r'(?:".*?")|(?:\'.*?\')\s*',
    'comment': r'!.*$',
    'delimiter': r',\s*',
    'continuation': r'&\s*',
    'spaces': r'\s+',
    'open_array': r'\[\s*',
    'close_array': r'\]\s*',
    'open_group': r'\(\s*',
    'close_group': r'\)\s*',
    'text': r'[^][)(\'",!&\s]+\s*'}
LINE_REGEX = r'|'.join(LINE_REGEXES)
LINE_PATTERN = re.compile(LINE_REGEX, re.IGNORECASE)

#===================== HELPER FUNCTIONS =======================================#

# LAMBDA FUNCTIONS

# Check if s has matching quotation marks
is_quoted = lambda s: s[0] in '"\''"'" and s[-1] == s[0]

# Check if s has the form of a Fortran array
is_array = lambda s: (s[0], s[-1]) == ('[', ']')

# Remove quotation marks or brackets (outer pair)
remove_delimiters = lambda s: s[1:-1] if is_quoted(s) or is_array(s) else s

# Make a divider column comment ('#') using character c with length n
make_divider = lambda c, n: r"#" + max(0, n-1)*c

# Return n newlines
newlines = lambda n: max(n, 0)*newline

# Verify final non-space character is AMP signaling complete line for joining
complete = lambda s: not s.strip().endswith(AMP)

# Strip space characters and final AMP
strip_amp = lambda s: s.strip().strip(AMP)

# Transpose a matrix (Sequence of Sequences) assuming equal len Sequences
transpose = lambda m: list(zip(*m))

# Get columns widths of str table (str matrix)
widths = lambda t: [max(len(r.strip()) for r in c)+2 for c in transpose(t)]

# Pad all str table (matrix) row values for equal width columns
pad_all = lambda t: transpose([r.ljust(w) for r in c] for c, w in zip(transpose(t), widths(t)))

# Get component identifier from command-line arguments or input filename
get_component = lambda a: a.component if a.component else Path(a.input).stem if a.input else None

# REGULAR FUNCTIONS

def update(d, t):
    """ Update dict d entry with (key, value) tuple """
    d[t[0]].append(t[1])
    return d

def pop_push(sequence, element, start=True):
    """ Pop element in sequence and push onto end """
    if element in sequence:
        sequence.remove(element)
        if start:
            sequence.insert(0, element)
            return sequence
        sequence.append(element)
    return sequence

def unique(iterable):
    """ Get unique elements in an iterable """
    seen = set()
    for element in filterfalse(seen.__contains__, iterable):
        seen.add(element)
    return seen

def strip_comment(s):
    """ Return the portion of a code line before any comment """
    re_commented = re.compile(CODE_RE, re.IGNORECASE)
    return re_commented.match(s)[1]

def join_line(line, lines=[BLANK]):
    """ Join line to last line in lines and check for completeness """
    if len(lines) == 0:
        lines.append(BLANK)
    lines[-1] = lines[-1] + strip_amp(line)
    if complete(line):
        lines.append(BLANK)
    return lines

#================================ PARSING =====================================#

def parse_line(line, skipped=None):
    """ Return state and arguments, if line of text matches pattern"""
    m=SUBROUTINE_PATTERN.match(line)
    if m:
        return (m.group('state'), m.group('arguments'))
    elif skipped is not None:
        skipped.append(line)
    return (None, None)

join_lines = lambda lines: reduce(lambda a, c: join_line(c, a), (strip_comment(l) for l in lines), [])

def parse_file(args, sio=None, skipped=None):
    """ Read, join, parse, and return list of (state, dict[arguments]) """
    with (sio if sio else open(args.input, 'r')) as f:
        lines = f.readlines()
        joined = reduce(lambda a, c: join_line(c, a), (strip_comment(l) for l in lines), [])
    return joined

def make_records(lines, skipped=None):
    calls = (parse_line(line, skipped) for line in lines)
    calls = ((state.upper(), args_str) for state, args_str in calls if state)
    return [(state, parse_call(args_str, *REQ_ARGS)) for state, args_str in calls]

def parse_call(string, *keys):
    """ Return dict[argument name, argument value] from argument str """
    equals = r'='
    strip = methodcaller('strip')
    upper = methodcaller('upper')
    split = methodcaller('split', equals)
    has_eq = lambda a: equals in a
    no_eq = lambda a: not has_eq(a)
    def format_tuple(t):
        a, b = t
        return upper(a), remove_delimiters(b)
    def strip_split(s):
        return map(strip, split(s))
    def parse_positional(pos):
        return map(strip, takewhile(no_eq, pos))
    def parse_keyword(kw):
        return map(strip_split, takewhile(has_eq, dropwhile(no_eq, kw)))
    pos, kw = tee(string.split(r','))
    pos = list(map(format_tuple, zip(keys, parse_positional(pos))))
    kw = list(map(format_tuple, parse_keyword(kw)))
    return dict(filterfalse(lambda t: t[0] in IGNORE_COLUMNS, chain(pos, kw)))

def make_output(state, records):
    """ Return list of output (str) lines for state from records with header """
    key_filter = lambda k: k.upper() != 'RC'
    cols = [d.keys() for d in records]
    cols = list(chain.from_iterable(cols))
    cols = unique(cols)
    cols = list(cols)
    cols = sorted(list(filter(key_filter, cols)))
    for col in LAST_COLUMNS:
        cols = pop_push(cols, col, start=False)
    for col in reversed(FIRST_COLUMNS):
        cols = pop_push(cols, col)
    rows = [[r.get(c, BLANK) for c in cols] for r in records]
    rows.insert(0, cols)
    lines = [RS.join(row) for row in pad_all(rows)]
    llen = max([len(line) for line in lines])
    divider = make_divider(r'-', llen-1)
    cols_line, *lines = lines
    return [f'category: {state}', divider, cols_line, divider, *lines]

def parse_args():
    description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("input", nargs=r'?', default=None, action='store', help="source Gridded Component filename")
    parser.add_argument("-o", "--output", action='store', help="destination specs filename")
    parser.add_argument("-c", "--component", action='store', default=None, help="component name")
    parser.add_argument("-d", "--debug", action='store', default=None, help="debug log")
    # Flag to run in testing mode for development troubleshooting
    parser.add_argument("-t", "--run-tests", action='store_true', default=False, help="run internal tests")
    return parser.parse_args()

def convert(component, args=None, test_data=None, skipped=None):
    """ Convert source (file or internal) to ACG spec files lines """
    if not(args or test_data) or (args and test_data):
        return None
    # Parse file of Fortran code as specified in command-line arguments
    lines = parse_file(args, sio=test_data, skipped=skipped)
    # Make records from lines
    records = make_records(lines, skipped=skipped)
    # Make dict using state as key and the records for that state as value
    state_records = reduce(update, records, defaultdict(list))
    # Make output sections by state
    sections = [newline.join(l) for l in (make_output(*sr) for sr in sorted(state_records.items(), key=lambda t: t[0]))]
    # Output acg spec file
    return f'schema_version: 2.0.0{newline}component: {component}{newlines(2)}{newlines(2).join(sections)}{newline}'

#================================== TESTING ===================================#

TEST_DATA = (
r"""    call MAPL_AddImportSpec  (                                   &
                              LONG_NAME  = 'air_pressure',       &
                              SHORT_NAME = 'PLE',                &
                              GC         = GC,                   &
                              UNITS      = 'Pa',                 &
                              DIMS       = MAPL_DimsHorzVert,    &
                              VLOCATION  = MAPL_VLocationEdge,   &
                              RC         = STATUS)


    call MAPL_AddExportSpec  (GC,                                &
                              SHORT_NAME = 'U',                  &
                              LONG_NAME  = 'eastward_wind',      &
                              UNITS      = 'm s-1',              &
                              DIMS       = MAPL_DimsHorzVert,    &
                              VLOCATION  = MAPL_VLocationCenter, &
                              RC         = STATUS)


    call MAPL_AddInternalSpec(GC,                                &
                              SHORT_NAME = 'PKZ',                &
                              LONG_NAME  = 'pressure_to_kappa',  &
                              UNITS      = 'Pa$^\kappa$',        &
                              PRECISION  = ESMF_KIND_R8,         &
                              DIMS       = MAPL_DimsHorzVert,    &
                              VLOCATION  = MAPL_VLocationCenter, &
                              RC         = STATUS)""",
r"""schema_version: 2.0.0
component: acg_writer_test

category: EXPORT
#-------------------------------------------------------------------------------------
SHORT_NAME   | UNITS   | DIMS                | VLOCATION              | LONG_NAME      
#-------------------------------------------------------------------------------------
U            | m s-1   | MAPL_DimsHorzVert   | MAPL_VLocationCenter   | eastward_wind  

category: IMPORT
#----------------------------------------------------------------------------------
SHORT_NAME   | UNITS   | DIMS                | VLOCATION            | LONG_NAME     
#----------------------------------------------------------------------------------
PLE          | Pa      | MAPL_DimsHorzVert   | MAPL_VLocationEdge   | air_pressure  

category: INTERNAL
#----------------------------------------------------------------------------------------------------------------
SHORT_NAME   | UNITS         | DIMS                | VLOCATION              | LONG_NAME           | PRECISION     
#----------------------------------------------------------------------------------------------------------------
PKZ          | Pa$^\kappa$   | MAPL_DimsHorzVert   | MAPL_VLocationCenter   | pressure_to_kappa   | ESMF_KIND_R8  """
)

#==============================================================================#

def verification(test_data, error_msgs=None):
    component = 'acg_writer_test'
    a, b = test_data
    results = convert(component, test_data=StringIO(a))
    result_lines = results.splitlines()
    b_lines = b.splitlines()
    if len(b_lines) != len(result_lines):
        if error_msgs is not None:
            error_msgs.append('Number of lines differs')
            error_msgs.append('')
        return FAILURE
    pairs = zip(result_lines, b_lines)
    for rl, bl in pairs:
        if rl == bl:
            continue
        if error_msgs is not None:
            error_msgs.append('Lines differ')
            error_msgs.append(f'"{bl.strip()}"')
            error_msgs.append(f'"{rl.strip()}"')
            error_msgs.append('')
        return FAILURE
    return SUCCESS

def test(args, error_msgs=None):
    """ Test functions for development """
    return verification(TEST_DATA, error_msgs)

#==================================== MAIN ====================================#

def main(args):
    """ Write acg spec file based on Fortran code """
    """ args is dict of arguments: {input: ..., component: ..., output: ...} """
    rc = FAILURE
    error_msgs = []
    # Run tests if run_tests parameter is present
    if args.run_tests:
        rc = test(args, error_msgs)
        if rc == FAILURE:
            sys.stderr.write('Tests failed.\n')
            for line in error_msgs:
                sys.stderr.write(line)
        else:
            sys.stderr.write('Success.\n')
        return rc
    if not args.input:
        sys.stderr.write('input filename missing.\n')
        return rc
    # Array for lines that are skipped for debugging
    skipped = [] if args.debug else None
    # Get gridded component name
    component = get_component(args)
    # Convert source code
    results = convert(component, args=args, skipped=skipped)
    # Write ACG spec file
    with (open(args.output, 'w') if args.output else sys.stdout) as f:
        f.writelines(results)
    # If not debugging or no skipped lines, return
    if not (args.debug and (skipped)):
        return SUCCESS
    # Otherwise, write out skipped lines
    with open(args.debug, 'w') as f:
        rc = SUCCESS
        if skipped:
            rc = FAILURE
            f.writelines(f'{line}{newline}' for line in skipped)
        return rc

# Main
if __name__ == '__main__':
    # Get command-line arguments
    args = parse_args()
    # Make acg specs file
    rc = main(args)

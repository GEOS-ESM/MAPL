#!/usr/bin/env python3
import re
import argparse
import sys
from functools import reduce, partial
from pathlib import Path
from itertools import filterfalse, chain
from collections import defaultdict
from os import linesep as newline

SUCCESS = 0
FAILURE = SUCCESS-1
BLANK = r''
RS = r' | '
AMP = r'&'
FIRST_COLUMNS = 'SHORT_NAME UNITS DIMS VLOCATION'.split()
LAST_COLUMNS = 'LONG_NAME'.split()

SUBROUTINE_RE = re.compile(r'call\s+MAPL_Add(?P<state>\w+)Spec\s*\((?P<arguments>.*)\)\s*$', re.IGNORECASE)

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

#==============================================================================#

is_quoted = lambda s: s[0] in '"\''"'" and s[-1] == s[0]
is_array = lambda s: (s[0], s[-1]) == ('[', ']')
remove_delimiters = lambda s: s[1:-1] if is_quoted(s) or is_array(s) else s
make_divider = lambda c, n: r"#" + max(0, n-1)*c
newlines = lambda n: max(n, 0)*newline
complete = lambda s: not s.strip().endswith(AMP)
strip_amp = lambda s: s.strip().strip(AMP)
def push_end(sequence, element):
    if element in sequence:
        sequence.remove(element)
        sequence.append(element)
    return sequence

#==============================================================================#

def unique(iterable):
    seen = set()
    for element in filterfalse(seen.__contains__, iterable):
        seen.add(element)
        yield element

#==============================================================================#

def strip_comment(s):
    re_commented = re.compile(r'\s*([^!]*)', re.IGNORECASE)
    return re_commented.match(s)[1]

#==============================================================================#

def join_line(line, lines=[BLANK]):
    if len(lines) == 0:
        lines.append(BLANK)
    lines[-1] = lines[-1] + strip_amp(line)
    if complete(line):
        lines.append(BLANK)
    return lines

#==============================================================================#

def parse_line(line, skipped=None):
    m=SUBROUTINE_RE.match(line)
    if m:
        return (m.group('state'), m.group('arguments'))
    elif skipped is not None:
        skipped.append(line)
    return (None, None)

#==============================================================================#

def parse_file(args, skipped=None):
    with open(args.input, 'r') as f:
        lines = f.readlines()
        joined = reduce(lambda a, c: join_line(c, a), (strip_comment(l) for l in lines), [])
    calls = (parse_line(line, skipped) for line in joined)
    calls = ((state.upper(), args_str) for state, args_str in calls if state)
    return [(state, parse_call(args_str)) for state, args_str in calls]

#==============================================================================#

def parse_call(args):
    _, short, *tail = [ap.strip().split('=') for ap in args.split(',')]
    tuples = [('SHORT_NAME', short[-1])] + [t for t in tail if len(t) == 2]
    return dict((k.strip().upper(), remove_delimiters(v.strip())) for k, v in tuples)

#==============================================================================#

def make_output(state, records):
    key_filter = lambda k: k.upper() != 'RC'
    cols = list(unique(chain.from_iterable(map(lambda d: d.keys(), records))))
    cols = list(filter(key_filter, cols))
    for col in LAST_COLUMNS:
        cols = push_end(cols, col)
    rows = [[r.get(c, BLANK) for c in cols] for r in records]
    rows.insert(0, cols)
    lines = [RS.join(row) for row in pad_all(rows)]
    llen = max([len(line) for line in lines])
    divider = make_divider(r'-', llen-1)
    cols_line, *lines = lines
    return [f'category: {state}', divider, cols_line, divider, *lines]

#==============================================================================#
transpose = lambda m: list(zip(*m))
widths = lambda t: [max(len(r.strip()) for r in c)+2 for c in transpose(t)]
pad_all = lambda t: transpose([r.ljust(w) for r in c] for c, w in zip(transpose(t), widths(t)))

#==============================================================================#

def make_parser():
    description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("input", action='store', help="source Gridded Component filename")
    parser.add_argument("-o", "--output", action='store', help="destination specs filename")
    parser.add_argument("-c", "--component", action='store', default=None, help="component name")
    parser.add_argument("-d", "--debug", action='store', default=None, help="debug log")
    parser.add_argument("--run-tests", action='store_true', default=False, help="run internal tests")
    return parser

def update(d, t):
    d[t[0]].append(t[1])
    return d

get_component = lambda a: a.component if a.component else Path(a.input).stem if a.input else None

#==============================================================================#

def test(args):
    regex = r'(?P<tag>regex)'
    line_code = r'y = sin(x) '
    commented_line = line_code + r'! Trig'
    uncommented_line = strip_comment(commented_line)
    if uncommented_line != r'y=sin(x)':
        print('Failure for strip_comment')
        return FAILURE
    return SUCCESS

def main(args):
    """ args is dict of arguments: {input: ..., component: ..., output: ...} """
    rc = FAILURE
    if args.run_tests:
        rc = test(args)
        return rc
    skipped = [] if args.debug else None
    component = get_component(args)
    records = parse_file(args, skipped)
    state_records = reduce(update, records, defaultdict(list))
    sections = [newline.join(l) for l in (make_output(*sr) for sr in sorted(state_records.items(), key=lambda t: t[0]))]
    with open(args.output, 'w') as f:
        f.write(f'schema_version: 2.0.0{newline}component: {component}{newlines(2)}')
        f.write(newlines(2).join(sections))
        f.write(newline)
    if not (args.debug and (skipped)):
        return SUCCESS
    with open(args.debug, 'w') as f:
        rc = SUCCESS
        if skipped:
            rc = FAILURE
            f.writelines(f'{line}{newline}' for line in skipped)
        return rc

# Main
if __name__ == '__main__':
    args = make_parser().parse_args()
    rc = main(args)
#==============================================================================#

#!/usr/bin/env python3
import re
import argparse
from functools import reduce, partial
from pathlib import Path
from itertools import filterfalse, chain
from collections import defaultdict
from os import linesep

SUCCESS = 0
FAILURE = SUCCESS-1
SPACE = r' '
BLANK = r''
BLANK_LINE = [BLANK]
COMMENT = r'#'
FORTCOMM = r'!'
DASH = r'-'
RS = r' | '
SUBROUTINE_RE = re.compile(r'call\s+MAPL_Add(?P<state>\w+)Spec\s*\((?P<arguments>.*)\)\s*$', re.IGNORECASE)

#==============================================================================#

is_quoted = lambda s: s[0] in '"\''"'" and s[-1] == s[0]
is_array = lambda s: (s[0], s[-1]) == ('[', ']')
remove_delimiters = lambda s: s[1:-1] if is_quoted(s) or is_array(s) else s
make_comment = lambda s: f"{COMMENT}{s}"
make_divider = lambda c, n: make_comment(f"{c * max(0, n-1)}")
newline = linesep
newlines = lambda n: max(n, 0)*newline

#==============================================================================#

def unique(iterable):
    seen = set()
    for element in filterfalse(seen.__contains__, iterable):
        seen.add(element)
        yield element

#==============================================================================#

def general_strip_comment(c, s):
    re_commented = re.compile(f'^(?P<code>[^{c}]*?)(?:{c}.*?)?$')
    m = re_commented.match(s)
    if m:
        return m.group('code')
    return s

strip_comment = partial(general_strip_comment, FORTCOMM)

#==============================================================================#

def join_line(line, lines=BLANK_LINE):
    stripped = line.strip()
    complete = not stripped.endswith('&')
    if len(lines) == 0:
        lines.append(BLANK)
    lines[-1] = lines[-1] + stripped.strip('&')
    if complete:
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
    joined = reduce(lambda a, c: join_line(c, a), lines, [])
    calls = (parse_line(line, skipped) for line in joined)
    calls = [(state.upper(), args_str) for state, args_str in calls if state]
    return [(state, parse_call(args_str)) for state, args_str in calls]

#==============================================================================#

def parse_call(args):
    _, short, *tail = [ap.strip().split('=') for ap in args.split(',')]
    tuples = [('SHORT_NAME', short[-1])] + [t for t in tail if len(t) == 2]
    return dict((key.strip().upper(), remove_delimiters(value.strip())) for key, value in tuples)

#==============================================================================#

def make_output(state, records):
    key_filter = lambda k: k != 'RC'
    cols = list(filter(key_filter, unique(chain.from_iterable(map(lambda d: d.keys(), records)))))
    rows = [[r.get(c, BLANK) for c in cols] for r in records]
    rows.insert(0, cols)
    lines = [RS.join(row) for row in pad_all(rows)]
    llen = max([len(line) for line in lines])
    divider = make_divider(DASH, llen-1)
    cols_line, *lines = lines
    return [f'category: {state}', divider, cols_line, divider, *lines]

#==============================================================================#
def pad(s, w):
    d = w - len(s.strip())
    return (d//2)*SPACE + s.strip() + (d//2 + d%2)*SPACE

transpose = lambda m: list(zip(*m))
widths = lambda t: [max(len(r.strip()) for r in c)+2 for c in transpose(t)]
pad_all = lambda t: transpose([pad(r, w) for r in c] for c, w in zip(transpose(t), widths(t)))

#==============================================================================#

def make_parser():
    description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("input", action='store', help="source Gridded Component filename")
    parser.add_argument("-o", "--output", action='store', help="destination specs filename")
    parser.add_argument("-c", "--component", action='store', default=None, help="component name")
    parser.add_argument("-d", "--debug", action='store', default=None, help="debug log")
    return parser

def update(d, t):
    d[t[0]].append(t[1])
    return d

def prepend(value, iterable):
    "Prepend a single value in front of an iterable."
    return chain([value], iterable)

#==============================================================================#

def main(args):
    """ args is dict of arguments: {input: ..., component: ..., output: ...} """
    rc = FAILURE
    skipped = [] if args.debug else None
    component = args.component if args.component else Path(args.input).stem if args.input else None
    records = parse_file(args, skipped)
    state_records = reduce(update, records, defaultdict(list))
    sections = [newline.join(l) for l in (make_output(*sr) for sr in sorted(state_records.items(), key=lambda t: t[0]))]
    with open(args.output, 'w') as f:
        f.write(f'schema_version: 2.0.0{newline}component: {component}{newlines(2)}')
        f.write(newlines(2).join(sections))
    rc = SUCCESS
    if not (args.debug and (skipped)):
        return rc
    with open(args.debug, 'w') as f:
        if skipped:
            f.writelines(f'{line}{newline}' for line in skipped)

# Main
if __name__ == '__main__':
    parser = make_parser()
    args = parser.parse_args()
    main(args)

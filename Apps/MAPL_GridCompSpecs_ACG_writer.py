#!/usr/bin/env python3
import re
import argparse
from functools import reduce, partial
from pathlib import Path
from itertools import filterfalse, chain

COMMENT = r'#'
FORTCOMM = r'!'
DASH = r'-'
RS = r' | '
SUBROUTINE_RE = re.compile(r'call\s+MAPL_Add(?P<state>\w+)Spec\s*\((?P<arguments>.*?)\)', re.IGNORECASE)

is_quoted = lambda s: (s[0] == "'" or s[0] == '"') and s[-1] == s[0]
is_array = lambda s: (s[0], s[-1]) == ('[', ']')
remove_delimiters = lambda s: s[1:-1] if is_quoted(s) or is_array(s) else s
make_comment = lambda s: f"{COMMENT}{s}"
make_divider = lambda c, n: make_comment(f"{c * max(0, n-1)}"[:max(0, n-1)])

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

def join_line(line, lines=['']):
    stripped = line.strip()
    complete = not stripped.endswith('&')
    if len(lines) == 0:
        lines.append('')
    lines[-1] = lines[-1] + stripped.strip('&')
    if complete:
        lines.append('')
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

is_quoted = lambda s: (s[0] == "'" or s[0] == '"') and s[-1] == s[0]
is_array = lambda s: (s[0], s[-1]) == ('[', ']')
remove_delimiters = lambda s: s[1:-1] if is_quoted(s) or is_array(s) else s

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
    rows = list(map(lambda r: map(lambda c: r.get(c, ''), cols), records))
    llen = max_row_len([cols]+rows, RS)
    divider = make_divider(DASH, llen-1)
    header = [f'category: {state}', divider, RS.join(list(cols)), divider]
    rows = [RS.join(list(v)) for v in ((r.get(c, '') for c in cols) for r in records)]
    return header + rows + ['']

#==============================================================================#

def column_widths(table):
    return tuple(max(len(r) for r in col) for col in zip(table))

#==============================================================================#

def max_row_len(table, delim=None):
    delim_len = len(delim)*(len(table[0])-1) if delim else 0
    return max(tuple(map(lambda r: sum(tuple(map(len, r))), table))) + delim_len

#==============================================================================#

def make_parser():
    description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("input", action='store', help="source Gridded Component filename")
    parser.add_argument("-o", "--output", action='store', help="destination specs filename")
    parser.add_argument("-c", "--component", action='store', default=None, help="component name")
    parser.add_argument("-d", "--debug", action='store', default=None, help="debug log")
    return parser

#==============================================================================#

def main(args):
    """ args is dict of arguments: {input: ..., component: ..., output: ...} """
    skipped = [] if args.debug else None
    component = args.component if args.component else Path(args.input).stem if args.input else None
    records = parse_file(args, skipped)
    lines = ['schema_version: 2.0.0', f'component: {component}', '']
    for state in set(s for s, _ in records):
        state_records = [r for s, r in records if s == state]
        lines.extend(make_output(state, state_records))
    with open(args.output, 'w') as f:
        f.writelines(f'{line}\n' for line in lines)
    if skipped:
        with open(args.debug, 'w') as f:
            f.writelines(f'{line}\n' for line in skipped)

# Main
if __name__ == '__main__':
    parser = make_parser()
    args = parser.parse_args()
    main(args)

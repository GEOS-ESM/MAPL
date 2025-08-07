#!/usr/bin/env python3
import re
import argparse
from functools import reduce, partial
from pathlib import Path
from itertools import filterfalse

COMMENT = r'#'
FORTCOMM = r'!'
SUBROUTINE_RE = re.compile(r'call\s+MAPL_StateAdd(?P<state>\w+)Spec\((?P<arguments>.*?)\)')

is_quoted = lambda s: (s[0] == "'" or s[0] == '"') and s[-1] == s[0]
is_array = lambda s: (s[0], s[-1]) == ('[', ']')
remove_delimiters = lambda s: s[1:-1] if is_quoted(s) or is_array(s) else s

#==============================================================================#

def make_line(strings, delimiter, leader = '', spacing = ' '):
    return leader + (spacing + delimiter + spacing).join(strings)

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

def joinline(line, lines=['']):
    stripped = line.strip()
    complete = not stripped.endswith('&')
    if len(lines) == 0:
        lines.append('')
    lines[-1] = lines[-1] + stripped.strip('&')
    if complete:
        lines.append('')
    return lines

SUBROUTINE_RE = re.compile(r'call\s+MAPL_StateAdd(?P<state>\w+)Spec\((?P<arguments>.*?)\)')

def parse_line(line):
    m=SUBROUTINE_RE.match(line)
    if m:
        return (m.group('state'), m.group('arguments'))
    return (None, None)

#==============================================================================#

def make_line(strings, delimiter, leader = '', spacing = ' '):
    return leader + (spacing + delimiter + spacing).join(strings)

#==============================================================================#

is_quoted = lambda s: (s[0] == "'" or s[0] == '"') and s[-1] == s[0]
is_array = lambda s: (s[0], s[-1]) == ('[', ']')
remove_delimiters = lambda s: s[1:-1] if is_quoted(s) or is_array(s) else s

#==============================================================================#

def parse_file(args):
    with open(args.input, 'r') as f:
        lines = f.readlines()
    joined = reduce(lambda a, c: joinline(c, a), lines, [])
    calls = (parse_line(line) for line in joined)
    calls = [(state.upper(), args_str) for state, args_str in calls if state]
    return [(state, parse_call(args_str)) for state, args_str in calls]

#==============================================================================#

def parse_call(args):
    _, short, *tail = [ap.strip().split('=') for ap in args.split(',')]
    tuples = [('SHORT_NAME', short[-1])] + [t for t in tail if len(t) == 2]
    return dict((key.strip().upper(), remove_delimiters(value.strip())) for key, value in tuples)

#==============================================================================#


def make_output(state, records):
    COMMENT = '#'
    lines = [f'category: {state}']
    names = reduce(lambda a, c: a.extend(c.keys()), records)
    column_names = list(filter(lambda i: i in set(names), names))
    vals = ((record.get(name, '') for name in column_names) for record in records)
    column_line = make_line(filter(lambda i: i in set(names), names), '|')
    vals_lines = [ make_line(v, '|') for v in vals ]
    width = len(column_line)
    maxwidth = max(max(map(len, vals_lines)), width)
    lines.append(f'{COMMENT}{'-' * (maxwidth-1)}')
    lines.append(column_line)
    lines.append(f'{COMMENT}{'-' * (maxwidth-1)}')
    lines.extend(vals_lines)
    lines.append('')

    return lines

#==============================================================================#

def make_parser():
    description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("input", action='store', help="source Gridded Component filename")
    parser.add_argument("-o", "--output", action='store', help="destination specs filename")
    parser.add_argument("-c", "--component", action='store', default=None, help="component name")
    return parser

def main(args):
    """ args is dict of arguments: """
    """     args.input """
    """     args.component """
    """     args.output """

    # Build records from file
    component = args.component if args.component else Path(args.input).stem if args.input else None
    records = parse_file(args)
    lines = ['schema_version: 2.0.0', f'component: {component}', '']
    for state in set(s for s, _ in records):
        state_records = [r for s, r in records if s == state]
        lines.extend(make_output(state, state_records))
    with open(args.output, 'w') as f:
        f.writelines(f'{line}\n' for line in lines)

# Main
if __name__ == '__main__':
    parser = make_parser()
    args = parser.parse_args()
    main(args)

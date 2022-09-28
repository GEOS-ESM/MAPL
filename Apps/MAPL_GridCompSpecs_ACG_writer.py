#!/usr/bin/env python3
import re
import csv
import argparse
from functools import reduce

# Characters for parsing and output
DELIMITER = '|'
LINE_CHAR = '-'
CONTINUATION_CHARACTER = '&'
OUTPUT_COMMENT = '#'

# Keys for parsing
CATEGORY_KEY = 'CATEGORY'

# Leader for output of category
CATEGORY_LEADER = 'category: '

# Keys for required fields
REQ_KEYS = [ 'SHORT_NAME', 'UNITS', 'DIMS', 'VLOCATION' ]

FILE_HEADER = "schema_version: 2.0.0\ncomponent:"

# Headings for groups of fields
SPECS_HEADINGS = ["VARIABLE", "DIMENSIONS", "ADDITIONAL METADATA"]

# Missing header for specs that cannot be processed
MISSING_HEADER = OUTPUT_COMMENT + " These specs are missing one or more required fields."

# Regular expression for parsing the beginning of MAPL_App calls
# with group names to extract portions of the line
call_re = re.compile('^call\s+MAPL_Add(?P<category>\w*?)Spec\((?P<remainder>.*)$', re.I)

#==============================================================================#

def make_line(strings, delimiter, leader = '', spacing = ' '):
    """ Return leader + strings joined by spacing+delimiter+spacing """
    return leader + (spacing + delimiter + spacing).join(strings)

#==============================================================================#

def strip_char(line, char):
    """ Strip all characters (including char) from first instance of char """
    # If char was found, strip to end; else line
    n = line.find(char)
    return line if n < 0 else line[:n]

#==============================================================================#

def strip_continue(line):
    """ Strip line continuation. """
    return strip_char(line, CONTINUATION_CHARACTER)

#==============================================================================#

def strip_comment(line):
    """ Strip inline comment. """
    INPUT_COMMENT = '!'
    return strip_char(line, INPUT_COMMENT)

#==============================================================================#

def add_to_tuple_dict(el, dct, key):
    """ Add element el tuple dict dct by key. """

    # Work on copy to avoid side effects (assuming values are immutable).
    cpy = dct.copy()

    t = el,

    # If key is found in dct, append el to cpy[key] value.
    if key in cpy:
        t = cpy[key] + t

    # Either way, update/add cpy[key]
    cpy[key] = t
    return cpy

#==============================================================================#

def unquote(s):
    """ Strip single and double quotes. """
    return ''.join(''.join(s.split('"')).split("'"))

#==============================================================================#

def multifind(string, substring, offset = 0):
    """ Return arrray of indices of all instances of substring in string """
    n = string.find(substring, offset)
    return [] if n < 0 else ([n] + multifind(string, substring, n+1))

#==============================================================================#

def pad_to_width(strings, widths, pad = ' ', center = False, ljust = True):
    """ Return strings in iterable strings padded to widths """
    fcenter =   lambda s, w: s.center(w, pad)
    fleft   =   lambda s, w: s.ljust(w, pad)
    fright  =   lambda s, w: s.rjust(w, pad)
    if center:
        f = fcenter
    elif ljust:
        f = fleft
    else:
        f = fright
    return [ f(s,w) for (s, w) in zip(strings, widths) ]

#==============================================================================#

def all_counts_equal(a):
    """ Return True if all iterables (including strings) have the same len. """
    """ a is an iterable of iterables. """
    return all(map(lambda b: b == len(a[0]), map(len, a))) if len(a) else False

#==============================================================================#

def get_max_lens(a):
    """ Return max len for each element in an iterable of iterables """
    """ a is an iterable of iterables. """
    max_lens = None
    if all_counts_equal(a):
        alllens = [ len_items(b) for b in a ]
        max_lens = list(reduce(lambda c, d: map(max, zip(c,d)), alllens))
    return max_lens

#==============================================================================#

def max_it(a, b):
    """ Return element by element max between a and b """
    return [ max(c,d) for (c, d) in zip(a, b) ]

#==============================================================================#

def len_items(a):
    """ Return length of each element in a """
    return [ len(b) for b in a ]

#==============================================================================#

def write_lines(filename, lines):
    """ Write lines (strings) in list to file """
    with open(filename, 'w') as specs:
       for line in lines:
           specs.write(line + '\n')

#==============================================================================#

def parse_file(filename):
    """ Parse input source code file into dict of tuples of strings ."""

    # Dictionary of tuples
    records = {}

    with open(filename, 'r') as code:

        # This is line built by concatenating lines based on continuation.
        joined = ''

        # Initially the category is not set. This serves as
        # a flag that parsing a MAPL_Add call has begun.
        category = None

        # read first line of file
        line = code.readline()

        # Keep reading lines until end of file
        while line:

            # Strip end of line comments. Then strip leading/trailing spaces.
            line = strip_comment(line).strip()

            # If line was only comment and spaces, skip.
            if line:

                # category is None => not a parsing a MAPL_Add call
                if category is None:

                    # Parse line with regex.
                    m = call_re.match(line)

                    # match found (MAPL_Add call), start parsing new MAPL_Add.
                    if m:

                        # Extract category.
                        category = m.group('category').upper()

                        # line becomes the remaining part of the line
                        line = m.group('remainder')

                    # line does not begin a MAPL_Add call, so read next line,
                    # because not already parsing a call (category is None)
                    else:
                        line = code.readline()
                        continue

                # category must be set at this point.

                # Concatenate line to joined after stripping any line continuation
                joined = joined + strip_continue(line).strip()

                # If line DOES NOT have a continuation, add string (record).
                # No continuation indicates end of (concatenated) line.
                if line.find(CONTINUATION_CHARACTER) < 0:
                    records = add_to_tuple_dict(joined, records, category)

                    # Initialize for next record
                    joined = ''
                    category = None

            # Read next line from file
            line = code.readline()

    return records

#==============================================================================#

def parse_records(records):
    """ Parse records (strings) to: """
    """ dict's of fields {field name: field value} (specs), """
    """ unique field names by category """
    """ and specs missing required fields (missing) """

    # specs keyed by category
    specs = {}

    # dict of all the unique keys for each category
    all_keys = {}

    # Records that are missing required fields
    missing = []

    for category in records:

        # Get all records for the current category.
        recs = records[category]

        # list of all valid specs for current category
        category_specs = []

        # Unique keys for current category
        unique_keys = set()

        # csv reader for recs
        reader = csv.reader(recs, skipinitialspace=True)

        # spec is text strings split from lines (strings) by csv_reader
        for spec in reader:

            # fields of spec
            fields = {}

            # each col is either a single string or this form string1 = string2
            for col in spec:
                # Split on '='
                kv = col.split('=')

                # first element is the key (field name)
                k = unquote(kv[0].strip().upper())

                # Continue if it is 'RC=...' which is irrelevant.
                if k == 'RC':
                    continue

                # If not, parse if there are two strings ('field=value')
                # String without '=' is an input variable, not a field
                if len(kv) > 1:

                    # Second string becomes field value
                    v = kv[1]

                    # Use first string as field name and add to fields
                    fields[k] = unquote(v.strip())

                    # Add key (field name) to list of unique keys (field names)
                    unique_keys = unique_keys | set([k])

            # All fields for current record have been parsed.

            # if fields.keys() contains all the required keys (field names),
            # add current fields to sequence of specs (dict's) for category.
            if set(fields.keys()) >= set(REQ_KEYS):

                # Add fields for 1 spec.
                category_specs.append(fields)

            # Some required keys are missing so put in missing list.
            else:
                #  Add category as a field.
                fields[CATEGORY_KEY] = category
                missing.append(fields)

        # Add current specs (dicts) keyed by category.
        specs[category] = category_specs

        # Add unique keys keyed by category.
        all_keys[category] = unique_keys

    return (specs, all_keys, missing)

#==============================================================================#

def make_output(specs, all_keys, missing):
    """ Return string of lines from specs (dict's) and missing records (strings) """

    # These are the lines (strings) for the output.
    lines = []

    lines.append(FILE_HEADER)
    lines.append('')

    # For each category, get keys (field names) for the category.
    # Build header from keys. Create string for each spec.
    for category in specs:

        # Get specs for category.
        category_specs = specs[category]

        # Category line.
        lines.append(CATEGORY_LEADER + category.upper())

        # Get keys with required keys first in order.
        keys = list(REQ_KEYS) + sorted(list(all_keys[category] - set(REQ_KEYS)))

        # Make keys upper case just in case (no pun intended)
        keys = [k.upper() for k in keys]

        # Get field values
        vals = [ [ (spec[k] if k in spec else '') for k in keys ] for spec in category_specs ]

        # Find the maximum field length for each field in a list.
        field_widths = max_it(len_items(keys), get_max_lens(vals))

        # Pad the keys with spaces and build line of field headings.
        keys = pad_to_width(keys, field_widths, center = True)
        key_line = make_line(keys, DELIMITER)

        # Build field type headings line
        indices = multifind(key_line, DELIMITER)
        widths = [indices[1]-1, indices[3]-indices[1]-1, len(key_line)-indices[3]-1]
        type_headings = pad_to_width(SPECS_HEADINGS, widths, center = True)
        type_line = make_line(type_headings, DELIMITER, leader = OUTPUT_COMMENT, spacing = '')

        # Build horizontal line divider
        horz_line = OUTPUT_COMMENT + ' ' + LINE_CHAR * (len(type_line) - 2)

        # Build field value lines (records).
        vals = [pad_to_width(v, field_widths) for v in vals]
        vals_lines = [ make_line(v, DELIMITER) for v in vals ]

        # Add lines to list.
        lines.append(horz_line)
        lines.append(type_line)
        lines.append(horz_line)
        lines.append(key_line)
        lines.append(horz_line)
        lines.extend(vals_lines)
        lines.append('')

    # Strings for incomplete (missing required fields.)
    if missing:
        lines = lines + [ MISSING_HEADER ] + [OUTPUT_COMMENT + m for m in missing]

    return lines

#==============================================================================#

# Main

# Build command line parser
description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
parser = argparse.ArgumentParser(description=description)
parser.add_argument("input", action='store', help="source Gridded Component filename")
parser.add_argument("output", action='store', help="destination specs filename")

# Parse command line
args = parser.parse_args()

# Build records from file
records = parse_file(args.input)

# Process records to specs (dicts) for 1 spec per valid record
(specs, all_keys, missing) = parse_records(records)

# Print output
output = make_output(specs, all_keys, missing)
write_lines(args.output, output)

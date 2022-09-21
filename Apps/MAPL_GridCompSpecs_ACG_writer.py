#!/usr/bin/env python3
import re
import csv
import argparse

# Characters for parsing and output
BLANK = " "
OUT_DEL = ' | '
LINE_CHAR = '-'
CONT_CHAR = '&'
IN_COMM_CHAR = '!'
OUT_COMM_CHAR = '#'

# Keys for parsing
CATEGORY_KEY = 'CATEGORY'
SHORT_NAME_KEY = 'SHORT_NAME'
UNITS_KEY = 'UNITS'
DIMS_KEY = 'DIMS'
VLOC_KEY = 'VLOCATION'

# Leader for output of category
CATEGORY_LEADER = 'category : '

REQ_COLUMNS = ( (SHORT_NAME_KEY, 'NAME'),
                (UNITS_KEY, 'UNITS'),
                (DIMS_KEY, 'DIMS'),
                (VLOC_KEY, 'VLOC' ))

# Keys for required fields
REQ_KEYS = [k for k, _ in REQ_COLUMNS]

FILE_HEADER = """schema_version: 2.0.0
component: GOCART2G"""

# Headings for field types for output
HEAD_VAR = "VARIABLE"
HEAD_DIMS = "DIMENSIONS"
HEAD_META = "ADDITIONAL METADATA"

# Heading line for heading types
SPECS_HEADING = ( OUT_COMM_CHAR + BLANK + HEAD_VAR + OUT_DEL +
    HEAD_DIMS + OUT_DEL + HEAD_META )

# Heading line for required fields
SPECS_COLUMNS = OUT_DEL.join([h for _, h in REQ_COLUMNS])

# Horizontal line divider in output
HORZ_LINE = ( OUT_COMM_CHAR + BLANK +
    LINE_CHAR * (len(SPECS_HEADING) - len(OUT_COMM_CHAR + BLANK)) )

# Missing header
MISSING_HEADER = OUT_COMM_CHAR + " These specs are missing one or more required fields."

# Regular expression for parsing the beginning of MAPL_App calls
# with group names to extract portions of the line
call_re = re.compile('^call\s+MAPL_Add(?P<category>\w*?)Spec\((?P<remainder>.*)$', re.I)

def strip_char(line, char):
    """ Strip all characters (including char) from first instance of char """
    n = line.find(char)
    if n < 0:
        # char was not found, so return line
        return line
    else:
        # char was found, so strip to end
        return line[:n]

def strip_continue(line):
    """ Strip from CONT_CHAR to end of line """
    return strip_char(line, CONT_CHAR)

def strip_comment(line):
    return strip_char(line, IN_COMM_CHAR)

def cmp_insensitive(string, strlist):
   return string.upper() in [s.upper() in strlist]

def add_to_tuple_dict(el, dct, key):
    """ Add element el tuple dictionary dct by key. """
    # Work on copy to avoid side effects (assuming values are immutable).
    cpy = dct.copy()
    # Make 1 element tuple from el
    t = el,
    # If key is found in dct, append el to cpy[key] value.
    if key in cpy:
        t = cpy[key] + t
    # Either way, update/add cpy[key]
    cpy[key] = t
    return cpy

def make_field_string(spec, keys, empty):
        return OUT_DEL.join([(spec[k] if k in spec else empty) in keys])

# Main

# Build command line parser
description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
parser = argparse.ArgumentParser(description=description)
parser.add_argument("input", action='store', help="source Gridded Component filename")

# Parse command line
args = parser.parse_args()

# Parse input file for MAPL_Add calls concatenating continued lines into strings (records)
records = {}
filename = args.input

with open(filename, 'r') as code:
    # This is line being built by concatenating lines based on continuation.
    joined = ''
    # Initially the category is not set. This serves as a flag that parsing a
    # MAPL_Add call has begun.
    category = None
    # read first line of file
    line = code.readline()
    # Keep reading lines until end of file
    while line:
    # Strip comments on the end of line and then strip leading/trailing spaces
        line = strip_comment(line).strip()
        # If line is only comment and spaces, skip
        if line:
            # If the category has not been set yet, parse line with regex
            if category is None:
                m = call_re.match(line)
                # If a match is found (MAPL_Add call), extract category.
                if m:
                    category = m.group('category').upper()
                    # line becomes the remaining part of the line
                    line = m.group('remainder')
                # The line does not begin a MAPL_Add call, so read next line.
                else:
                    line = code.readline()
                    continue

            # Concatenate line to joined after stripping any line continuation
            joined = joined + strip_continue(line).strip()

            # If line DOES NOT have a continuation, add string (record).
            if line.find(CONT_CHAR) < 0:
                records = add_to_tuple_dict(joined, records, category)

                # Initialize for next record
                joined = ''
                category = None

        # Read next line from file
        line = code.readline()

# Process records to specs (dicts) for 1 spec per valid record
# Records that are missing required fields
missing = []

# specs keyed by category
specs = {}

# dict of all the unique keys for each category
all_keys = {}

for category in records:
    # Get all records for the current category.
    recs = records[category]
    # list of all valid specs for current category
    category_specs = []
    # Unique keys for current category
    unique_keys = set()
    # csv reader for recs
    reader = csv.reader(recs, skipinitialspace=True)
    for spec in reader:
    # fields of spec
       fields = {}
    # each col is either a single string or this form string1 = string2
       for col in spec:
           kv = col.split('=')
           # first element is the key (field name)
           k = kv[0].strip().upper()
           # Continue if it is 'RC=...'
           if k == 'RC':
               continue
           # If not, parse if there are two strings ('field=value')
           if len(kv) > 1:
               # Second string becomes field value
               v = kv[1]
               # Use first string as field name and add to fields
               fields[k] = v.strip() 
               unique_keys = unique_keys | set([k])


       # if fields.keys() contains all the required keys
       if set(fields.keys()) >= set(REQ_KEYS):
           # Add fields for 1 spec.
           category_specs.append(fields)
       else:
       # Some required keys are missing so put in missing list.
           # First category as a field.
           fields[CATEGORY_KEY] = category
           missing.append(fields)
           # Create list of missing keys
           # missing_keys = ', '.join(set(REQ_KEYS) - fields.keys())
           # Report problem
           # print("{} missing for spec".format(missing_keys))

    # Add specs keyed by category.
    specs[category] = category_specs
    # Add unique keys keyed by category.
    all_keys[category] = unique_keys

# Print file
lines = []
lines.append(FILE_HEADER)
lines.append('')

for category in specs:
    # Get keys with required keys first in order.
    keys = list(REQ_KEYS) + sorted(list(all_keys[category] - set(REQ_KEYS)))

    # Make keys upper case just in case (no pun intended)
    keys = [k.upper() for k in keys]

    # Get specs for category.
    category_specs = specs[category]

    # Print category.
    lines.append(CATEGORY_LEADER + category.upper())
    lines.append(HORZ_LINE)

    # Print heading for field types
    lines.append(SPECS_HEADING)
    lines.append(HORZ_LINE)

    # Print field headings.
    line = ' ' + OUT_DEL.join(keys)
    lines.append(line)
    lines.append(HORZ_LINE)

    # Create string of fields for each spec.
    for spec in category_specs:
        vals = [(spec[k] if k in spec else BLANK) for k in keys]
        line = ' ' + OUT_DEL.join(vals)
        lines.append(line)

    lines.append('')

if missing:
    lines.append(MISSING_HEADER)
    for spec in missing:
        lines.append(IN_COMM_CHAR + spec)

output = "\n".join(lines)
print(output)

#!/usr/bin/env python3
import re
import csv
import argparse
from functools import reduce

# Characters for parsing and output
BLANK = " "
OUTPUT_DELIMITER = ' | '
LINE_CHAR = '-'
CONTINUATION_CHARACTER = '&'
INPUT_COMMENT_CHARACTER = '!'
OUTPUT_COMMENT_CHARACTER = '#'

# Keys for parsing
CATEGORY_KEY = 'CATEGORY'

# Leader for output of category
CATEGORY_LEADER = 'category : '

# Keys for required fields
REQ_KEYS = [ 'SHORT_NAME', 'UNITS', 'DIMS', 'VLOCATION' ]

FILE_HEADER = "schema_version: 2.0.0\ncomponent:"

# Heading line for heading types
SPECS_HEADING = ( OUTPUT_COMMENT_CHARACTER + BLANK + "VARIABLE" +
    OUTPUT_DELIMITER + "DIMENSIONS" + OUTPUT_DELIMITER + "ADDITIONAL METADATA" )

# Horizontal line divider in output
HORZ_LINE = ( OUTPUT_COMMENT_CHARACTER + BLANK +
    LINE_CHAR * (len(SPECS_HEADING) - len(OUTPUT_COMMENT_CHARACTER + BLANK)) )

# Missing header
MISSING_HEADER = OUTPUT_COMMENT_CHARACTER + " These specs are missing one or more required fields."

# Regular expression for parsing the beginning of MAPL_App calls
# with group names to extract portions of the line
call_re = re.compile('^call\s+MAPL_Add(?P<category>\w*?)Spec\((?P<remainder>.*)$', re.I)

#==============================================================================#

def strip_char(line, char):
    """ Strip all characters (including char) from first instance of char """
    n = line.find(char)
    if n < 0:
        # char was not found, so return line
        return line
    else:
        # char was found, so strip to end
        return line[:n]

#==============================================================================#

def strip_continue(line):
    """ Strip line continuation. """
    return strip_char(line, CONTINUATION_CHARACTER)

#==============================================================================#

def strip_comment(line):
    """ Strip inline comment. """
    return strip_char(line, INPUT_COMMENT_CHARACTER)

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

def make_field_string(spec, keys, empty):
    """ Make a string from the fields of a spec """
    return OUTPUT_DELIMITER.join([(spec[k] if k in spec else empty) in keys])

#==============================================================================#

def unquote(s):
    """ Strip single and double quotes. """
    return ''.join(''.join(s.split('"')).split("'"))

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

        # Get keys with required keys first in order.
        keys = list(REQ_KEYS) + sorted(list(all_keys[category] - set(REQ_KEYS)))

        # Make keys upper case just in case (no pun intended)
        keys = [k.upper() for k in keys]

        # Get specs for category.
        category_specs = specs[category]

        # Category line.
        lines.append(CATEGORY_LEADER + category.upper())
        lines.append(HORZ_LINE)

        # Heading for field types
        lines.append(SPECS_HEADING)
        lines.append(HORZ_LINE)

        # field headings.
        line = ' ' + OUTPUT_DELIMITER.join(keys)
        lines.append(line)
        lines.append(HORZ_LINE)

        # string of fields for each spec.
        for spec in category_specs:
            vals = [(spec[k] if k in spec else BLANK) for k in keys]
            line = ' ' + OUTPUT_DELIMITER.join(vals)
            lines.append(line)

        lines.append('')

    # Strings for incomplete (missing required fields.)
    if missing:
        lines = lines + [ MISSING_HEADER ] + [INPUT_COMMENT_CHARACTER + m for m in missing]

    # Join lines and return
    return "\n".join(lines)

#==============================================================================#

def get_string_lengths(itb_itb_str):
    """ Given  """
    maxlens = reduce(map(len, itb_str)


#==============================================================================#

# Main

# Build command line parser
description = 'Generate import/export/internal config specs file for MAPL Gridded Component'
parser = argparse.ArgumentParser(description=description)
parser.add_argument("input", action='store', help="source Gridded Component filename")

# Parse command line
args = parser.parse_args()

# Build records from file
records = parse_file(args.input)

# Process records to specs (dicts) for 1 spec per valid record
(specs, all_keys, missing) = parse_records(records)

# Print output
print(make_output(specs, all_keys, missing))

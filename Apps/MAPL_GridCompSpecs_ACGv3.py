#!/usr/bin/env python3
import argparse
import sys
from os.path import splitext, basename
from os import linesep
import csv
from collections.abc import Sequence
from functools import partial, reduce

################################# CONSTANTS ####################################
SUCCESS = 0
ERROR = SUCCESS - 1
DELIMITER = ', '
EMPTY = ''
AMP = '&'
SPACE = " "
SIZE_INDENT = 3
TERMINATOR = '_RC)'
UNIT = ()
INDENT = SPACE * SIZE_INDENT

ARGDICT = 'argdict' 
AS = 'as'
CONSTANTS = 'constants'
CONTROL = 'control'
CONTROLS = 'controls'
DEALIASED = 'dealiased'
FLAGS = 'flags'
FROM = 'from'
GC_ARGNAME = 'gridcomp'
IF_BLOCK = 'if_block'
INTENT_PREFIX = 'ESMF_STATEINTENT_'
MANDATORY = 'mandatory'
MAPPED = 'mapped' 
MAPPING = 'mapping'
MISSING_MANDATORY = 'missing_mandatory'
SPEC = 'spec'
SPECIFICATIONS = 'specifications'
SPECS_NOT_FOUND = 'specs_not_found'
SPEC_ALIASES = 'spec_aliases'
STORE = 'store'
STRING = 'string'
VALUES_NOT_FOUND = 'values_not_found'

#could be read from YAML list
ALIAS = 'alias'
ALLOC = 'alloc'
ARRAY = 'array'
CONDITION = 'condition'
DIMS = 'dims'
INTENT_ARG = 'intent_arg' 
INTERNAL_NAME = 'internal_name'
MANGLED = 'mangled'
STANDARD_NAME_ARG = 'standard_name_arg'
PRECISION = 'precision'
RANK = 'rank'
SHORT_NAME = 'short_name'
SHORT_NAME_ARG = 'short_name_arg'
STANDARD_NAME = 'standard_name'
STATE = 'state'
STATES = 'states'
STATE_ARG = 'state_arg'
STATE_INTENT = 'state_intent'
STRINGVECTOR = 'string_vector'
UNGRIDDED_DIMS = 'ungridded_dims'
VSTAGGER = 'vstagger'

# command-line option constants
GC_VARIABLE = 'gridcomp_variable'
GC_VARIABLE_DEFAULT = 'gc'
LONGNAME_GLOB_PREFIX = "longname_glob_prefix" # Should add alias for cmd option wdb
# procedure names
ADDSPEC = "MAPL_GridCompAddFieldSpec"
GETPOINTER = "MAPL_StateGetPointer"
TO_STRING_VECTOR = "toStringVector"
# Fortran keywords
CALL = 'call'
# constants for logicals
FALSE_VALUE = '.false.'
TRUE_VALUE = '.true.'
TRUE_VALUES = {'t', 'true', 'yes', 'y'}
# identity function (id is a builtin function, so this is capitalized.)
ID = lambda x: x

##################################### FLAGS ####################################
def get_set(o):
    match o:
        case set() as s:
            return s
        case str():
            return {o}
        case None:
            return set()
        case _:
            return set(o)

def has_flags(has_all, flags, option):
    if not isinstance(option, dict):
        return False
    oflags = get_set(option.get(FLAGS))
    cflags = get_set(flags)
    return cflags.issubset(oflags) if has_all else not cflags.isdisjoint(oflags)

is_mandatory = lambda o: has_flags(has_all=True, flags=MANDATORY, option=o)
is_printable = lambda o: not has_flags(has_all=False, flags={STORE, CONTROL}, option=o) if o else False
has_as_flag = lambda o: has_flags(has_all=True, flags=AS, option=o)

#################################### OPTIONS ###################################
""" dict for the possible options in a spec, as well as command-line arguments,
constants, and aliases
options: dict[str, dict[str, *]]
    * can be:
    dict[str,  dict[str, **]]: spec values
        ** can be a simple scalar type or a Sequence, set, or dict
    dict[str, str]: command-line options
    list[str]: constants
    dict[str, str]: aliases
"""
def get_options(args):
    states = ['import', 'export', 'internal']
    intents = [f"{INTENT_PREFIX}{state.upper()}" for state in states]
    options = {}
    options[SPECIFICATIONS] = { 
        DIMS: {FLAGS: {MANDATORY}, MAPPING: { 
            'z': "'z'",
            'xy': "'xy'",
            'xyz': "'xyz'",
            'MAPL_DimsVertOnly': "'z'",
            'MAPL_DimsHorzOnly': "'xy'",
            'MAPL_DimsHorzVert': "'xyz'"}},
        SHORT_NAME: {MAPPING: MANGLED, FLAGS: MANDATORY},
        STATE_INTENT: {FLAGS: {MANDATORY}}, 
        STANDARD_NAME: {FLAGS: MANDATORY}, 
        PRECISION: {}, 
        UNGRIDDED_DIMS: {MAPPING: ARRAY}, 
        VSTAGGER: {MAPPING: { 
             'C': 'VERTICAL_STAGGER_CENTER',
             'E': 'VERTICAL_STAGGER_EDGE',
             'N': 'VERTICAL_STAGGER_NONE'}},
        ALIAS: {FLAGS: {STORE}}, 
        ALLOC: {FLAGS: {STORE}}, 
        'attributes' : {MAPPING: STRINGVECTOR}, 
        CONDITION: {FLAGS: {STORE}}, 
        'dependencies': {MAPPING: STRINGVECTOR}, 
        'itemtype': {}, 
        'orientation': {}, 
        'regrid_method': {}, 
        STATE: {FLAGS: {MANDATORY, STORE}}, 
        'typekind': {MAPPING: { 
            'R4': 'ESMF_Typekind_R4',
            'R8': 'ESMF_Typekind_R8',
            'I4': 'ESMF_Typekind_I4',
            'I8': 'ESMF_Typekind_I8'}},
        'units': {MAPPING: STRING}, 
        'vector_pair': {MAPPING: STRING} 
        }

    options[SPEC_ALIASES] = { 
        'ungrid': UNGRIDDED_DIMS,
        'ungridded': UNGRIDDED_DIMS,
        'cond': CONDITION,
        'long name': STANDARD_NAME,
        'long_name': STANDARD_NAME,
        'name': SHORT_NAME,
        'prec': PRECISION,
        'vloc': VSTAGGER,
        'vlocation': VSTAGGER
    }

    options[CONTROLS] = {IF_BLOCK: {MAPPING: IF_BLOCK, FLAGS: {CONTROL, AS}, FROM: CONDITION}} 

    options[ARGDICT] = vars(args) 

    options[MAPPED] = { 
        STANDARD_NAME_ARG: {MAPPING: STANDARD_NAME, FROM: (STANDARD_NAME, LONGNAME_GLOB_PREFIX), AS: STANDARD_NAME}, 
        INTENT_ARG: {FROM: (STATE_INTENT, STATE), MAPPING: (ID, dict(zip(states, intents))), FLAGS: AS},
        RANK: {MAPPING: RANK, FLAGS: {STORE, MANDATORY}, FROM: (DIMS, UNGRIDDED_DIMS)}, 
        STATE_ARG: {FROM: (STATE, STATE_INTENT), MAPPING: (ID, dict(zip(intents, states))), FLAGS: AS} 
    }

    options[CONSTANTS] = {STATES: states}

    return options

# Procedures for writing to files
def emit_specs(values, options):
    emitted = emit_args(values, flatten_options(options))
    if condition := values.get(CONDITION):
        return condition(emitted)
    return emitted

def emit_args(values, options):
    gc_variable = options[GC_VARIABLE]
    columns = [c for c in values if is_printable(options.get(c))]
    lines = [f"{INDENT}{AMP} {column}={values[column]}{DELIMITER}{AMP}" for column in columns]
    return [f"{CALL} {ADDSPEC}({GC_ARGNAME}={gc_variable}, {AMP}", *lines, f"{INDENT}{AMP} {TERMINATOR}"]

def emit_declare_pointers(values):
    name = values[INTERNAL_NAME]
    rank = values[RANK]
    kind = f'(kind={values[PRECISION]})' if PRECISION in values else EMPTY
    middle = ',:'*(rank-1)
    return f'real{kind}, pointer, dimension(:{middle}) :: {name}'

def emit_get_pointers(values):
    internal_name = values[INTERNAL_NAME]
    condition = values.get(CONDITION)
    parts = [f'{CALL} {GETPOINTER}({values[STATE]}', internal_name, values[SHORT_NAME]]
    if alloc := values.get(ALLOC):
        parts.append(f'{ALLOC}={convert_to_fortran_logical(alloc)}')
    line = DELIMITER.join([*parts, TERMINATOR])
    return condition([line], make_else_block(internal_name)) if condition else [line]

############################ PARSE COMMAND ARGUMENTS ###########################
def get_args():
    parser = argparse.ArgumentParser(description='Generate FieldSpecs, pointer declarations, and get_pointer calls for MAPL Gridded Component')
    parser.add_argument("input", action='store',
                    help="input filename")
    parser.add_argument("-n", "--name", action="store",
                    help="override default grid component name derived from input filename")
    parser.add_argument("-i", "--import_specs", action="store", nargs='?',
                    default=None, const="{component}_Import___.h",
                    help="override default output filename for AddImportSpec() code")
    parser.add_argument("-x", "--export_specs", action="store", nargs='?',
                    default=None, const="{component}_Export___.h",
                    help="override default output filename for AddExternalSpec() code")
    parser.add_argument("-p", "--internal_specs", action="store", nargs='?',
                    default=None, const="{component}_Internal___.h",
                    help="override default output filename for AddImportSpec() code")
    parser.add_argument("-g", "--get-pointers", action="store", nargs='?',
                    default=None, const="{component}_GetPointer___.h",
                    help="override default output filename for get_pointer() code")
    parser.add_argument("-d", "--declare-pointers", action="store", nargs='?',
                    const="{component}_DeclarePointer___.h", default=None,
                    help="override default output filename for pointer declaration code")
    parser.add_argument("--" + LONGNAME_GLOB_PREFIX, dest=LONGNAME_GLOB_PREFIX,
                    action="store", nargs='?', default=None,
                    help="alternative prefix for long_name substitution")
    parser.add_argument(f"--{GC_VARIABLE}", dest=GC_VARIABLE,
                    action="store", nargs='?', default=GC_VARIABLE_DEFAULT,
                    help="ESMF_GridComp variable name")
    return parser.parse_args()


# READ_SPECS function
def read_specs(specs_filename):
    """Returns dict of (state_intent: list of dict of (option name: option value) """
    def csv_record_reader(csv_reader):
        """ Read a csv reader iterator until a blank line is found. """
        prev_row_blank = True
        for row in csv_reader:
            if not (len(row) == 0):
                if row[0].startswith('#'):
                    continue
                yield [cell.strip() for cell in row]
                prev_row_blank = False
            elif not prev_row_blank:
                return

    def dataframe(reader, columns):
        """ Read a reader iterator and return a list of dictionaries, each including column name and value. """
        df = []
        for row in reader:
            df.append(dict(zip(columns, row)))
        return df

    def add_state(d, state):
        d[STATE] = state
        return d

# Python is case sensitive, so dict lookups are case sensitive.
# The column names are Fortran identifiers, which are case insensitive.
# So all lookups in the dict below should be converted to lowercase.
    specs = {}
    with open(specs_filename, 'r') as specs_file:
        specs_reader = csv.reader(specs_file, skipinitialspace=True,delimiter='|')
        gen = csv_record_reader(specs_reader)
        schema_version = next(gen)[0].split(' ')[1]
        component = next(gen)[0].split(' ')[1]
        while True:
            try:
                gen = csv_record_reader(specs_reader)
                _, state = next(gen)[0].lower().split()
                columns = [c.strip().lower() for c in next(gen)]
                df = dataframe(gen, columns)
                specs[state] = [add_state(d, state) for d in df]
            except StopIteration:
                break

    return specs

def get_from_keys(option):
    match option.get(FROM):
        case str() as k:
            return (k,)
        case tuple() | list() as s:
            return s

def get_from_values(keys, values, argdict):
    get_from_value = lambda k: values.get(k, argdict.get(k))
    match keys:
        case str() as key:
            value = get_from_value(key)
            return (value,)
        case tuple():
            return tuple(get_from_value(key) for key in keys)
        case _:
            raise RuntimeError('Option is not a supported type')

def digest_spec(spec, options):
    values = {}
    specs_not_found = []
    spec_options = options[SPECIFICATIONS]
    for spec_name, spec_value in spec.items():
        opt = spec_options.get(spec_name)
        if opt is None:
            specs_not_found.append(spec_name)
            continue
        m = fetch_mapping_function(opt.get(MAPPING))
        value = m(spec_value)
        name = opt.get(AS, spec_name)
        values[name] = value
    return values, specs_not_found

def map_spec_values(values, options):
    mapped_values = values
    argdict = options[ARGDICT]
    values_not_found = []
    value_types = list(filter(lambda k: k in {MAPPED, CONTROLS}, options.keys()))
    value_options = reduce(lambda a, t: a | options[t], value_types, {})
    for option_name, option in value_options.items():
        if not isinstance(option, dict):
            continue
        from_keys = get_from_keys(option)
        from_values = get_from_values(from_keys, mapped_values, argdict)
        first, *_ = from_keys
        mname = option.get(MAPPING)
        m = fetch_mapping_function(mname)
        mapped_values_value = m(*from_values)
        name = option.get(AS, first if has_as_flag(option) else option_name) 
        mapped_values[name] = mapped_values_value
    return mapped_values, values_not_found

def get_mandatory_option_keys(options):
    keys = []
    for tname, toptions in options.items():
        if tname in {SPEC_ALIASES, CONSTANTS}:
            continue
        for key, option in toptions.items():
            if is_mandatory(option):
                keys.append(key)
    return keys

def get_internal_name(spec):
    if spec is None:
        return None
    alias = spec.get(ALIAS, EMPTY).strip()
    return alias if alias else spec.get(SHORT_NAME, EMPTY).replace('*', EMPTY)

def get_values(specs, options):
    all_values = []
    results = []
    aliases = options[SPEC_ALIASES]
    flat_specs = flatten_specs(specs)
    for spec in flat_specs:
        dealiased = dict((aliases.get(k, k), v) for k, v in spec.items())
        internal_name = get_internal_name(dealiased)
        spec_values, specs_not_found = digest_spec(dealiased, options)
        values, values_not_found = map_spec_values(spec_values, options)
        # Because the internal name is used in declare and get_pointer, it is singled out here.
        values[INTERNAL_NAME] = internal_name
        all_values.append(values)
        mandatory_keys = get_mandatory_option_keys(options)
        missing_mandatory = set(mandatory_keys).difference(values.keys())
        result = {SPEC: spec,
                  DEALIASED: dealiased,
                  SPECS_NOT_FOUND: specs_not_found,
                  VALUES_NOT_FOUND: values_not_found,
                  MISSING_MANDATORY: missing_mandatory}
        results.append(result)
    return all_values, results

def flatten_specs(specs):
    match specs:
        case Sequence():
            flat_specs = list(specs)
        case dict():
            flat_specs = reduce(lambda a, c: a + c, specs.values(), [])
    return flat_specs
    
def flatten_options(o):
    flat = {}
    for v in o.values():
        flat.update(v)
    return flat 

################################# EMIT_VALUES ##################################
def emit_values(specs, options, args):

    argdict = options[ARGDICT]
    exit_code_ = ERROR

    add_newline = lambda s: f"{s.rstrip()}{linesep}"

    if args.name:
        component = args.name
    else:
        component, _ = splitext(basename(args.input))
        component = component.replace('_Registry','')
        component = component.replace('_StateSpecs','')

# open all output files
    f_specs = {}
    states = options[CONSTANTS][STATES]
    for state in states:
        option = args.__dict__[state + "_specs"]
        if option:
            fname = option.format(component=component)
            f_specs[state] = open_with_header(fname)
        else:
            f_specs[state] = None

    if args.declare_pointers:
        f_declare_pointers = open_with_header(args.declare_pointers.format(component=component))
    else:
        f_declare_pointers = None
    if args.get_pointers:
        f_get_pointers = open_with_header(args.get_pointers.format(component=component))
    else:
        f_get_pointers = None

# Generate code from specs (processed above)
    for state in states:
        state_specs = list(filter(lambda s: s[STATE] == state, specs))
        if state_specs:
            for values in state_specs:
                if f_specs[state]:
                    lines = [add_newline(line) for line in emit_specs(values, options)]
                    f_specs[state].writelines(lines)
                if f_declare_pointers:
                    emitted_declarations = emit_declare_pointers(values)
                    f_declare_pointers.write(add_newline(emitted_declarations))
                if f_get_pointers:
                    f_get_pointers.writelines(add_newline(line) for line in emit_get_pointers(values))

# Close output files
    for f in list(f_specs.values()):
        if f:
            f.close()
    if f_declare_pointers:
        f_declare_pointers.close()
    if f_get_pointers:
        f_get_pointers.close()

        return SUCCESS

############################### HELPER FUNCTIONS ###############################
def add_quotes(s):
    if s is None:
        return None
    return f"'{rm_quotes(s)}'"
mk_array = lambda s: '[ ' + str(s).strip().strip('[]') + ']' if s else None
rm_quotes = lambda s: s.replace('"', '').replace("'", '') if s else None
construct_string_vector = lambda value: f"{TO_STRING_VECTOR}({add_quotes(value)})" if value else None

def convert_to_fortran_logical(b):
     return TRUE_VALUE if b.strip().strip('.').lower() in TRUE_VALUES else FALSE_VALUE 

def compute_rank(dims, ungridded):
    RANK_LOOKUP = {"'z'": 1, "'xy'": 2, "'xyz'": 3}
    base_rank = RANK_LOOKUP.get(dims)
    if base_rank is None:
        return None
    extra_rank = len(ungridded.strip('][').split(',')) if ungridded else 0
    return base_rank + extra_rank

def header():
    """
    Returns a standard warning that can be placed at the top of each
    generated _Fortran_ include file.
    """

    return """
!                          -------------------
!                          W  A  R  N  I  N  G
!                          -------------------
!
!   This code fragment is automatically generated by MAPL_GridCompSpecs_ACG.
!   Please DO NOT edit it. Any modification made in here will be overwritten
!   next time this file is auto-generated. Instead, enter your additions
!   or deletions in the .rc file in the src tree.
!
"""

def open_with_header(filename):
    f = open(filename,'w')
    f.write(header())
    return f

def mangle_standard_name(name, prefix):
    if name is None:
        return None
    if name.startswith('*'):
        prefix_ = prefix if prefix else 'comp_name'
        name_ = add_quotes(name[1:])
        return f"trim({prefix_})//{name_}"
    return add_quotes(name)

def make_if_block(condition, text, else_block=[]):
    condition_line = f"if ({condition}) then{linesep}"
    conclusion = f"end if{linesep}"
    lines = [f"{INDENT}{line}{linesep}" for line in text] + else_block
    return [condition_line] + lines + [conclusion]
    
def make_else_block(name=None):
    if name:
        return [f'else{linesep}', f'{INDENT}nullify({name}){linesep}']
    return []

######################### WRITERS for writing AddSpecs #########################
NAMED_MAPPINGS = {
        STRING: lambda value: add_quotes(value),
        STRINGVECTOR: lambda value: construct_string_vector(value),
        ARRAY: lambda value: mk_array(value),
        MANGLED: lambda name: add_quotes(name.replace("*","'//trim(comp_name)//'")) if name else None,
        STANDARD_NAME: mangle_standard_name,
        RANK: compute_rank, 
        IF_BLOCK: lambda value: partial(make_if_block, value) if value else None
        }

def fetch_mapping_function(m, func_dict=NAMED_MAPPINGS):
    return make_mapping(m, func_dict=(func_dict if func_dict else NAMED_MAPPINGS))

def valid_index(seq, n):
    if isinstance(seq, Sequence) and isinstance(n, int):
        return n >= 0 and n < len(seq)
    return False

def make_mapping(m, func_sequence=None, func_dict=None):
    if m is None or m is UNIT:
        return lambda t: t
    elif callable(m):
        return m

    match m:
        case str() if func_dict:
            return func_dict.get(m)
        case dict():
            return lambda k: m.get(k)
        case int() if valid_index(func_sequence, m):
            return func_sequence[n]
        case list() if len(m) == 0:
            return None
        case tuple() | list() if len(m) > 0:
            funcs = tuple(make_mapping(sm, func_sequence=None, func_dict=None) for sm in m)
            def inner(*args):
                for f, arg in zip(funcs, args):
                    if arg is None:
                        continue
                    return f(arg)                                                                         
                return None
            return inner

# Main Procedure (Added to facilitate testing.)
def main():
    exit_code = ERROR

# Process command line arguments
    args = get_args()

# Get options
    required_keys = {SPECIFICATIONS, SPEC_ALIASES, CONTROLS, ARGDICT, MAPPED}
    options = get_options(args)
    missing_keys = required_keys.difference(options)
    intersection = required_keys.intersection(options)
    if missing_keys:
        raise RuntimeError(f"Some option types are missing: {missing_keys}")

# Process blocked CSV input file
    parsed_specs = read_specs(args.input)

    values, results = get_values(parsed_specs, options)
    missing = [(r[SPEC], r[MISSING_MANDATORY]) for r in results if r[MISSING_MANDATORY]]
    if missing:
        for s, n in missing:
            print(f"value for {n} is missing in spec {s}")
        exit_code = ERROR

# Emit values
    exit_code = emit_values(values, options, args)
    sys.exit(exit_code)

#############################################
# MAIN program begins here
#############################################

if __name__ == "__main__":
    main()
# FIN
    sys.exit(SUCCESS)

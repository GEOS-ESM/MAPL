#!/usr/bin/env python3
import argparse
import sys
import os
import csv
from collections import namedtuple
from collections.abc import Callable
import operator
from functools import partial, reduce
from graphlib import TopologicalSorter
from enum import IntFlag
from dataclasses import dataclass

################################# CONSTANTS ####################################
SUCCESS = 0
ERROR = SUCCESS - 1
NL = "\n"
DELIMITER = ', '
TERMINATOR = '_RC)'
# keys for options
ALIAS = 'alias'
ALLOC = 'alloc'
ARRAY = 'array'
AS = 'as'
CALCULATION = 'calculation'
CONDITION = 'condition'
CONTROL = 'control'
DIMS = 'dims'
FLAGS = 'flags'
FROM = 'from'
GC_ARGNAME = 'gridcomp'
IDENTITY = 'identity'
IF_BLOCK = 'if_block'
INTENT = 'state_intent'
INTENT_PREFIX = 'ESMF_STATEINTENT_'
INTERNAL_NAME = 'internal_name'
MANDATORY = 'mandatory'
MANGLED = 'mangled'
MANGLED_NAME = 'mangled_name'
MANGLED_STANDARD_NAME = 'mangled_standard_name'
MAPPING = 'mapping'
OUTPUT = 'output'
PARAMETERIZED = 'parameterized'
PRECISION = 'precision'
RANK = 'rank'
SHORT_NAME = 'short_name'
STANDARD_NAME = 'standard_name'
STATE = 'state'
STRING = 'string'
STORE = 'store'
STRINGVECTOR = 'string_vector'
UNGRIDDED_DIMS = 'ungridded_dims'
VSTAGGER = 'vstagger'
FLAG_NAMES = [MANDATORY, STORE, CONTROL]
NONPRINTABLE = {STORE, CONTROL}
STANDARD_NAME_MANGLE = 'mangle_standard'
RANK_MAPPING = 'rank_mapping'
MAKE_IF_BLOCK = 'make_if_block'

# command-line option constants
LONGNAME_GLOB_PREFIX = "longname_glob_prefix" # Should add alias for cmd option wdb
GC_VARIABLE_DEFAULT = 'gc'
GC_VARIABLE = 'gridcomp_variable'
# procedure names
ADDSPEC = "MAPL_GridCompAddFieldSpec"
GETPOINTER = "MAPL_GetPointer"
TO_STRING_VECTOR = "toStringVector"
# Fortran keywords
CALL = 'call'
# constants for logicals
TRUE_VALUE = '.true.'
FALSE_VALUE = '.false.'
TRUE_VALUES = {'t', 'true', 'yes', 'y', 'si', 'oui', 'sim'}
FALSE_VALUES = {'f', 'false', 'no', 'n', 'no', 'non', 'nao'}


##################################### FLAGS ####################################
def tuple_wrapper(v):
    match v:
        case str():
            return (v,)
        case set() | list():
            return tuple(v)
        case tuple():
            return v
        case None:
            return tuple()

OptionAttribute = IntFlag('OptionAttribute', FLAG_NAMES + 'VALUE SPEC COLUMN_ALIAS'.split())
def get_option_type(o):
    match o:
        case str():
            return OptionAttribute.COLUMN_ALIAS
        case dict():
            if FROM in o:
                return OptionAttribute.VALUE
            return OptionAttribute.SPEC
        case _:
            return None

def get_flags(o):
    flags = set()
    if FROM in o:
        for f in o[FROM]:
            try:
                a = OptionAttribute[f]
            except KeyError as ke:
                print(ke)
            else:
                flags.add(a)
    return flags
            
set_wrap = lambda v: {v} if isinstance(v, str) else set(v)
check_flags = lambda o, flags: not set_wrap(flags).isdisjoint(FLAGS) if o else None
is_mandatory = lambda o: check_flags(o, MANDATORY) if FLAGS in o else False
is_printable = lambda o: not check_flags(o, NONPRINTABLE) if FLAGS in o else True

#################################### OPTIONS ###################################
# dict for the possible options in a spec
@dataclass(frozen=True)
class FrozenKey:
   get_string: str

    @staticmethod
    def make_frozen_keys(*keys):
        return tuple(FrozenKey(key) in keys)

    @staticmethod
    def get_strings(*fks):
        return tuple(fk.get_string for fk in fks)

OPTIONS = {    
# MANDATORY
    DIMS: {FLAGS: {MANDATORY}, MAPPING: {
        'z': "'z'",
        'xy': "'xy'",
        'xyz': "'xyz'",
        'MAPL_DimsVertOnly': "'z'",
        'MAPL_DimsHorzOnly': "'xy'",
        'MAPL_DimsHorzVert': "'xyz'"
    }},
    INTENT: {FLAGS: {MANDATORY}, MAPPING: {
        'import': f'{INTENT_PREFIX}IMPORT',
        'export': f'{INTENT_PREFIX}EXPORT',
        'internal': f'{INTENT_PREFIX}INTERNAL'
    }},
    SHORT_NAME: {MAPPING: MANGLED, FLAGS: {MANDATORY}},
           #    STANDARD_NAME: {MAPPING: STANDARD_NAME_MANGLE, FROM: (STANDARD_NAME, LONGNAME_GLOB_PREFIX), FLAGS: MANDATORY},
    STANDARD_NAME: {FLAGS: (MANDATORY, STORE)},
# OPTIONAL
    PRECISION: {MAPPING: IDENTITY},
    UNGRIDDED_DIMS: {MAPPING: ARRAY},
    VSTAGGER: {MAPPING: {
         'C': 'VERTICAL_STAGGER_CENTER',
         'E': 'VERTICAL_STAGGER_EDGE',
         'N': 'VERTICAL_STAGGER_NONE',
    }},
    'attributes' : {MAPPING: STRINGVECTOR},
    'dependencies': {MAPPING: STRINGVECTOR},
    'itemtype': {MAPPING: IDENTITY},
    'orientation': {MAPPING: IDENTITY},
    'regrid_method': {MAPPING: IDENTITY},
    'typekind': {MAPPING: {
        'R4': 'ESMF_Typekind_R4',
        'R8': 'ESMF_Typekind_R8',
        'I4': 'ESMF_Typekind_I4',
        'I8': 'ESMF_Typekind_I8'
    }},
    'units': {MAPPING: STRING},
    'vector_pair': {MAPPING: STRING},
# aliases
    'ungrid': UNGRIDDED_DIMS,
    'ungridded': UNGRIDDED_DIMS,
    'cond': CONDITION,
    'long name': STANDARD_NAME,
    'long_name': STANDARD_NAME,
    'name': SHORT_NAME,
    'prec': PRECISION,
    'vloc': VSTAGGER,
    'vlocation': VSTAGGER,
# these are options that are not output but used to write 
# from specs
    ALIAS: {MAPPING: IDENTITY, FLAGS: {STORE}},
    CONDITION: {MAPPING: MAKE_IF_BLOCK, FLAGS: {CONTROL}},
    ALLOC: {FLAGS: {STORE}},
# from options
    MANGLED_STANDARD_NAME: {MAPPING: STANDARD_NAME_MANGLE, FROM: (STANDARD_NAME, LONGNAME_GLOB_PREFIX), AS: STANDARD_NAME},
    MANGLED_NAME: {MAPPING: MANGLED, FROM: SHORT_NAME, FLAGS: {STORE}},
    INTERNAL_NAME: {MAPPING: INTERNAL_NAME, FROM: (SHORT_NAME, ALIAS), FLAGS: {STORE}},
    RANK: {MAPPING: RANK_MAPPING, FLAGS: {STORE}, FROM: (DIMS, UNGRIDDED_DIMS)},
    STATE: {MAPPING: STATE, FROM: INTENT}
}

def get_option_key_order(options):
    nonalias_options = [(k, v) for (k, v) in options if isinstance(v, dict)]
    keys = [k for (k, _) in nonalias_options]
    ordered_keys = []
    dependent = lambda o: 
    dependent = lambda o: (if FROM in o else False) if isinstance(o, dict) else False 
    dependent_options = set([(k, v) for (k, v) in nonalias_options if FROM in v])
    ordered_keys = [k for (k, v) in (nonalias_options - dependent_options)]
    ordered_keys = ordered_keys + [k for (k, v) in dependent_options if (all(v[FROM] in ordered_keys) if isinst     ]

def get_ordered_option_keys(options):
    dependencies = []
    for key, option in options.items():
        fkeys = ()
        match option:
            case str():
                continue
            case {'from': keys}:
                match keys:
                    case str() as k:
                        fkeys = FrozenKey.make_frozen_keys((k,))
                    case tuple():
                        fkeys = FrozenKey.make_frozen_keys(*keys)
        dependencies.append(FrozenKey(key), fkeys)
    graph = dict(dependencies)
    ts = TopologicalSorter(graph)

    try:
        fkeys = ts.static_order()
    except CycleError() as ex:
        fkeys = None
        print('Options have a circular dependency: ', ex)
        raise ex
    return list(FrozenKey.get_strings(fkeys))

is_alias = lambda o: isinstance(o, str)

def newline(indent=0):
    return f'{NL}{" "*indent}'

###############################################################
# MAPL_DATASPEC class
class MAPL_DataSpec:
    """Declare and manipulate an import/export/internal specs for a
       MAPL Gridded component"""

    TERMINATOR = '_RC)'

    def __init__(self, spec_values, options, indent=3):
        self.spec_values = spec_values
        self.options = options
        self.indent = indent
        self.mangled_name = spec_values[MANGLED_NAME]
        self.internal_name = spec_values[INTERNAL_NAME]
        self.condition = spec_values.get(CONDITION)
        self.state_intent = spec_values[INTENT]

    def newline(self, indent=True):
        return newline(self.indent if indent else 0)

    def continue_line(self):
        return "&" + self.newline() + "& "

    def emit_specs(self):
        a = self.emit_args()
        indent = self.indent
        f = partial(self.condition, 3) if self.condition else lambda t: t
        return f(a) + NL
#        return self.emit_header() + self.emit_args() + self.emit_trailer(nullify=False)

    # Pointers must be declared regardless of COND status.  Deactivated
    # pointers should not be _referenced_ but such sections should still
    # compile, so we must declare the pointers
    def emit_declare_pointers(self):
        spec_values = self.spec_values
        rank, precision = (spec_values[RANK], spec_values.get(PRECISION, None))
        dimension = 'dimension(:' + ',:'*(rank-1) + ')'
        text = self.newline() + 'real'
        if precision:
            text = text + '(kind=' + str(precision) + ')'
        return text +', pointer, ' + dimension + ' :: ' + self.internal_name + self.newline()


    def emit_get_pointers(self):
        """ Generate MAPL_GetPointer calls for the MAPL_DataSpec (self) """
        """ Creates string by joining list of generated and literal strings """
        """ including if block (emit_header) and 'alloc = value' (emit_pointer_alloc) """

        indent = self.indent
        name = self.name
        a = DELIMITER.join([f'{CALL} {GETPOINTER}({self.state_intent}',
            self.internal_name, self.mangled_name] + self.emit_pointer_alloc() +
            [ TERMINATOR ])
        return self.condition(a, make_else_block(name, indent)) if self.condition else a

    def emit_pointer_alloc(self):
        EMPTY_LIST = []
        key = ALLOC
        value = self.spec_values.get(key)
        if value:
            value = value.strip().lower()
            listout = [ key + '=' + get_fortran_logical(value) ] if len(value) > 0 else EMPTY_LIST
        else:
            listout = EMPTY_LIST
        return listout

    def emit_header(self):
        text = self.newline()
        condition = self.condition
        if condition:
            self.indent = self.indent + 3
            text = text + "if (" + condition + ") then" + self.newline()
        return text

    def emit_args(self):
        self.indent = self.indent + 5
        text = f"{CALL} {ADDSPEC}({GC_ARGNAME}={GC_VARIABLE}, {self.continue_line()}"
        for column in self.spec_values:
            if is_printable(self.options[column]): #wdb idea deleteme reduce?
                text = text + self.emit_arg(column)
        text = text + TERMINATOR + self.newline()
        self.indent = self.indent - 5
        return text

    def emit_arg(self, column):
        value = self.spec_values.get(column)
        if value:
            text = f"{column}={value}{DELIMITER}{self.continue_line()}"
        else:
            text = ''
        return text

    def emit_trailer(self, nullify=False):
        if self.condition:
            self.indent = self.indent - 3
            name = self.internal_name
            text = self.newline()
            if nullify:
                text = text + "else" + self.newline()
                text = text + "   nullify(" + name + ")" + self.newline()
            text = text + "endif" + self.newline()
        else:
            text = self.newline()
        return text


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

    def dataframe(reader, columns, defaults):
        """ Read a reader iterator and return a list of dictionaries, each including column name and value. """
        df = []
        for row in reader:
            df.append(dict(zip(columns, row)))
        return df

    def add_state_intent(d, intent):
        if INTENT not in d:
            d[INTENT] = intent
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
                _, intent = next(gen)[0].lower().split()
                columns = [c.strip().lower() for c in next(gen)]
                df = dataframe(gen, columns, (INTENT, intent))
                #merged = [merge(row, defaults) for row in df]
                specs[intent] = [add_state_intent(d, intent) for d in df]
            except StopIteration:
                break

    return specs

# NEW DIGEST
def get_option(name, options, level=1):
    match options.get(name):
        case str() as real_name if level > 0:
            r = get_option(real_name, options, level-1)
        case dict() as d:
            r = (name, d)
        case _:
            raise RuntimeException(f"Unable to find '{name}' in options")
    return r
        
def dict_mapping(d):
    def mapping(v):
        if v in d:
            return d[v]
        if v in d.values():
            return v
        return None
    return mapping

def get_mapping(option, mappings):
    match option.get(MAPPING, IDENTITY):
        case str() as name:
            mapping = mappings.get(name)
        case Callable() as f:
            mapping = f
        case dict() as d:
            mapping = dict_mapping(d)
    if mapping is None:
        raise RuntimeError('Unable to get mapping')
    return mapping

def dealias(options, name, level=1):
    match options.get(name):
        case str() as real_name if level > 0:
            return dealias(options, real_name, level-1)
        case dict():
            return name
        case _:
            return None

def option_as(option, name):
    match option:
        case {'as': as_name}:
            return as_name
        case dict():
            return name

def get_from_values(option, values):
    match option:
        case str() as s:
            raise RuntimeError(f'Option is an alias: {s}')
        case dict() as d:
            from_keys = d.get(FROM)
            val = None
            match from_keys:
                case str() as from_key:
                    val = (values.get(from_key),)
                case tuple():
                    val = tuple(values.get(from_key) for from_key in from_keys)
            if val:
                return val
            raise RuntimeError('Unable to find value to map')
        case _:
            raise RuntimeError('Option is not a supported type')

def digest_spec(spec, options, keys, mappings, argdict):
    #get_as_name = partial(option, options)
    #get_name = partial(dealias, options)
#    spec_keys_found = [name for name in (get_name(key) for key in spec) if name]
#    spec_process_list = [(name, get_mapping(options[name]), spec[name]) for name in spec_keys_found]
#    spec_keys_not_found = set(spec.keys()).difference(spec_keys_found)
#    options_to_process = [(name, options[name]) for name in filter(lambda key: key not in spec_keys_found, keys)]
#    options_process_list = [(get_name, get_mapping(option), get_from_keys(option)) for (name, option) in
#         filter(lambda key: key not in spec_keys_found, keys) if name]
#    processed_spec = dict([(name, mapping(value)) for (name, mapping, value) in spec_process_list])
#    values = argdict | processed_spec


    values_found = list(argdict.keys())
    spec_keys_not_found = []
    spec_process_list = []
    options_process_list = []
    processed_spec = {}
    processed_options = {}

    for key in spec:
        name = dealias(options, key)
        if name is None:
            spec_keys_not_found.append(key)
            continue
        spec_process_list.append((name, get_mapping(options[name], mappings), spec[key]))
        values_found.append(name)
#    for key, name in values_found:
#        spec_process_list.append((name, spec[key], options[name]))
    #spec_keys_not_found = set(spec.keys()).difference(values_found)
#    processed_spec = dict([(name, mapping(value)) for (name, mapping, value) in spec_process_list])

    for (name, mapping, value) in spec_process_list:
        processed_spec[name] = mapping(value)
    values = argdict | processed_spec

    for key in keys:
        if key not in values_found:
            option = options[key]
            mapping = get_mapping(option, mappings)
            from_values = get_from_values(option, values)
            options_process_list.append((key, mapping, from_values))

    for key, mapping, from_values in options_process_list:
        processed_options[key] = mapping(*from_values)
    values = values | processed_options

    return values
    
#    processed_options = dict([(name, mapping(*from_values)) for (name, mapping, from_values) in
#         [  for (name, mapping, from_keys) in options_process_list]
#            ])
#    values = values | dict([(name, mapping(*)) for (name, from_values) in [(name,  ) for (name,  )

def digest(specs, options, keys, mappings, argdict):
#    is_alias = lambda k: isinstance(options[k], str) if k in options else False
    #get_name = partial(dealias, options)
    specvals = list(specs.values())
    specs = []
    for spec_list in specvals:
        for spec in spec_list:
            specs.append(spec)
#    specs = list[reduce(lambda a, c: a+c, list(specs.values()), [])]
    all_values = []
    for spec in specs:
        values = digest_spec(spec, options, keys, mappings, argdict)
        all_values.append(values)
    return all_values       
 #       spec_keys_found = []
 #       for key in spec:
 #           name = dealias(options, key)
 #           if name:
 #               spec_keys_found.append(name)
        #spec_keys_found = [name for name in (get_name(key) for key in spec) if name]
 #       spec_process_list = []
 #       for name in spec_keys_found:
 #           spec_process_list.append((name, spec[name], options[name]))
#        spec_process_list = [(name, spec[name], options[name]) for name in spec_keys_found]
#        spec_keys_not_found = set(spec.keys()).difference(spec_keys_found)
#        options_process_list = []
#        for key in keys:
#            if key in spec_keys_found:
#                continue
#            options_process_list.append((name, options[name]))
#        options_process_list = ([(name, options[name]) for name in
#             filter(lambda key: key not in spec_keys_found, keys)])
#        values = argdict


#    if(isinstance(specs, dict)):
#        specs = [reduce(lambda a, c: a+c if c else a, list(specs.values()), [])]
#    dealiased = [] # for comprehension
#    for spec in specs:
#        for k in filter(is_alias, list(spec)): # for comprehension
#            real_name = spec.pop(k)
#            spec[real_name] = spec[k] 
#        dealiased.append(spec)
#    for spec in dealiased:
#        values = argdict
#        for name, value in spec.items():
#            if is_alias(spec):
#                continue
#            option = options[name]
#            mapping = get_mapping(option, mappings)
#            values[name] = mapping(value)
#        for name in keys:
#            if name in values:
#                continue
#            name, option = get_option(name, options)
#            mapping = get_mapping(option, mapping)
#            match option.get(FROM):
#                case str() as fk:
#                    fromkeys = [fk]
#                case tuple() as fks:
#                    fromkeys = fks
#                case list() as fks:
#                    fromkeys = tuple(fks)
#                case _:
#                    raise RuntimeException(f"Unable to find values to map for '{name}'")
#            values[name] = mapping(*fromkeys)
#        all_values.append(values)
#    return all_values
# END DIGEST SPECS

################################# EMIT_VALUES ##################################
def emit_values(specs, args, options):

    add_newline = lambda s: f"{s.rstrip()}{NL}"

    if args.name:
        component = args.name
    else:
        component, _ = os.path.splitext(os.path.basename(args.input))
        component = component.replace('_Registry','')
        component = component.replace('_StateSpecs','')

    STATEINTENT_WRITER = options[INTENT][MAPPING]

# open all output files
    f_specs = {}
    for intent in STATEINTENT_WRITER.keys():
        option = args.__dict__[intent + "_specs"]
        if option:
            fname = option.format(component=component)
            f_specs[intent] = open_with_header(fname)
        else:
            f_specs[intent] = None

    if args.declare_pointers:
        f_declare_pointers = open_with_header(args.declare_pointers.format(component=component))
    else:
        f_declare_pointers = None
    if args.get_pointers:
        f_get_pointers = open_with_header(args.get_pointers.format(component=component))
    else:
        f_get_pointers = None

# Generate code from specs (processed above)
    for intent in STATEINTENT_WRITER.keys():
        f = lambda s: s[INTENT] == intent if INTENT in s else False
        intent_specs = filter(f, specs)
        if intent_specs:
            for spec_values in intent_specs:
                spec = MAPL_DataSpec(spec_values, options)
                if f_specs[intent]:
                    f_specs[intent].write(add_newline(spec.emit_specs()))
                if f_declare_pointers:
                    f_declare_pointers.write(add_newline(spec.emit_declare_pointers()))
                if f_get_pointers:
                    f_get_pointers.write(add_newline(spec.emit_get_pointers()))

# Close output files
    for f in list(f_specs.values()):
        if f:
            f.close()
    if f_declare_pointers:
        f_declare_pointers.close()
    if f_get_pointers:
        f_get_pointers.close()

############################### HELPER FUNCTIONS ###############################
none_check = lambda f: lambda v: f(v) if v else None
add_quotes = lambda s: f"'{str(s)}'" if s else None
mk_array = lambda s: '[ ' + str(s).strip().strip('[]') + ']' if s else None
construct_string_vector = lambda value: f"{TO_STRING_VECTOR}({add_quotes(value)})" if value else None

"""
def mangle_name_prefix(name, parameters = None):
    pre = 'comp_name'
    if isinstance(parameters, tuple):
        pre = parameters[0] if parameters[0] else pre
    codestring = f"'//trim({pre})/'" 
    return mappings[STRING](name.replace("*",codestring)) if name else None
"""

def get_fortran_logical(value_in):
    """ Return string representing Fortran logical from an input string """
    """ representing a logical value input """

    try:
        if value_in is None:
            raise ValueError("'None' is not valid for get_fortran_logical.")
        if value_in.strip().lower() in TRUE_VALUES:
            val_out = TRUE_VALUE
        elif value_in.strip().lower() in FALSE_VALUES:
            val_out = FALSE_VALUE
        else:
            raise ValueError("Unrecognized logical: " + value_in)
    except Exception:
        raise

    return val_out

def compute_rank(dims, ungridded):
    RANK_LOOKUP = {"'z'": 1, "'xy'": 2, "'xyz'": 3}
    base_rank = RANK_LOOKUP.get(dims)
    if base_rank is None:
        return None
    extra_rank = len(ungridded.strip('][').split(',')) if ungridded else 0
    return base_rank + extra_rank

def get_mandatory_options(options):
    return [name for name, value in options.items() if is_mandatory(value)]

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

# callable object (function)
class ParameterizedWriter:

    def __init__(self, writer, *parameter_keys):
        self.writer = writer
        self.parameter_keys = parameter_keys
        
    def __call__(self, name, parameters):
        parameter_values = tuple(parameters.get(key) for key in self.parameter_keys)
        return self.writer(name, parameter_values)

def get_state(intent):
    if intent is None:
        return None
    if intent.startswith(INTENT_PREFIX):
        return intent.remove_prefix(INTENT_PREFIX)

def mangle_standard_name(name, prefix):
    if name is None:
        return None
    if prefix is None:
        prefix='comp_name'
    return name.replace('*', f"//trim({prefix})//")

def get_internal_name(name, alias):
    return alias if alias else name.replace('*', '') if name else None

def make_if_block(condition, indent, text, else_block=''):
    indents = " "*indent
    return f"if ({condition}) then{NL}{indents}{text}{NL}{else_block}end if{NL}"
    
def make_else_block(name=None, indent=0):
    if name is None:
        return ''
    indents = " "*indent 
    return f'else{NL}{indents}nullify({name}){NL}'

######################### WRITERS for writing AddSpecs #########################
MAPPINGS = {
    STRING: lambda value: add_quotes(value),
    STRINGVECTOR: lambda value: construct_string_vector(value),
    ARRAY: lambda value: mk_array(value),
    MANGLED: lambda name: add_quotes(name.replace("*","'//trim(comp_name)//'")) if name else None,
    INTERNAL_NAME: lambda name, alias: get_internal_name(name, alias) if name else None,
#    PARAMETERIZED: ParameterizedWriter(mangle_name_prefix, LONGNAME_GLOB_PREFIX),
    STATE: get_state,
    IDENTITY: lambda value: value,
    STANDARD_NAME_MANGLE: mangle_standard_name,
    RANK_MAPPING: compute_rank, 
    MAKE_IF_BLOCK: lambda value: partial(make_if_block, value) if value else None 
}

# Main Procedure (Added to facilitate testing.)
def main():
# Process command line arguments
    args = get_args()
    argdict = vars(args)

# Process blocked CSV input file
    parsed_specs = read_specs(args.input)

# Get ordered option keys.
    try:
        keys = list(get_ordered_option_keys(OPTIONS))
    except Exception as ex:
        print(ex)
        #sys.exit(ERROR)
# Digest specs from file to output structure
    try:
        values = digest(parsed_specs, OPTIONS, keys, MAPPINGS, argdict)
    except Exception as ex:
        print(ex)
        #sys.exit(ERROR)

# Emit values
    emit_values(values, args, OPTIONS)

# Successful exit
    sys.exit(SUCCESS)

###################### RULES to test conditions on Options #####################
#fixme wdb RULES do not work because of MAPL3 changes. The functionality may be restored in a refactor.
# relations for rules on Options
def relation(relop, lhs, rhs, values):
    """ Returns the result of the relop relation of lhs and rhs using values for lookups """
    l = values[lhs] if isinstance(lhs, Option) else lhs
    r = values[rhs] if isinstance(rhs, Option) else rhs
    return relop(l, r)
        
# define common relations
equals = partial(relation, operator.eq)
does_not_equal = partial(relation, operator.ne)

# simple class to group information for a condition in a Rule
# compare option value against expected, produce logical value and message
condition = namedtuple('condition', 'option rel expected message')

class Rule:
    """ rule for testing conditions on Options  """

    @classmethod
    def predicate(cls, option, rel, expected):
        return partial(rel, option, expected)

    def __init__(self, conditions, joiner = all):
        """ creates rule conditions from tuples (conditions) joined by joiner function """
        """ set the check function (rule_check) """
        joiners = {all: (' and ', False), any: (' or ', True)}

        processed_conditions = tuple([condition(option, rel, expected, message) for option, rel, expected, message in conditions]) 

        # break_on_true sets behavior one condition is met
        try:
            rule_joiner, break_on_true = joiners[joiner]
        except KeyError:
            raise ValueError("Invalid joiner")
        
        def rule_check(values):
            messages = []
            results = []
            for next_condition in processed_conditions:
                option, rel, expected, message = next_condition
                test = Rule.predicate(option, rel, expected)
                test_result = test(values)
                results.append(test_result)
                if test_result:
                    # add message and break conditionally
                    messages.append(option.name_key + " " + message) 
                    if break_on_true:
                        break
                    
            if joiner(results) == True:
                raise RuntimeError(rule_joiner.join(messages))

        self.rule = rule_check
    
    def check(self, values):
        """ run rules on Option values """
        return self.rule(values)

# These are the CURRENT RULES of Option (column) values
def check_option_values(values):

    rules = [Rule(conditions = [(Option.DIMS, equals, 'MAPL_DimsHorzVert', 'is equal to MAPL_DimsHorzVert'),
            (Option.VLOCATION, equals, 'MAPL_VlocationNone', 'is equal to MAPL_VlocationNone')], joiner = all),
            Rule([condition(Option.DIMS, equals, 'MAPL_DimsHorzOnly', 'is equal to MAPL_DimsHorzOnly'),
            condition(Option.VLOCATION, does_not_equal, 'MAPL_VlocationNone', 'is not equal to MAPL_VlocationNone')])]

    for rule in rules:
        rule.check(values)
################################### END RULES ################################## 


#################################### UNUSED ####################################
# DIGEST
"""
def digest_(parsed_specs, args, options):
    # Set Option values from parsed specs #
    arg_dict = vars(args)
    mandatory_options = get_mandatory_options(options)
    digested_specs = dict()

#    mangle_option = options[MANGLED_NAME]
#    internal_option = options[INTERNAL_NAME]
    for state_intent in parsed_specs:
        category_specs = list() # All the specs for the state_intent
        for spec in parsed_specs[state_intent]: # spec from list
#            dims = None
#            ungridded = None
#            alias = None
            option_values = dict() # dict of option values
            for column in spec: # for spec writer value
                column_value = spec[column]
                option = options[column]
                if isinstance(option, str):
                    column = option
                    option = options[column]
                option_values[column] = map_value(column_value, option.get(MAPPING))
#                match option.get(MAPPING, IDENTITY):
#                    case dict() as d:
#                        k = column_value
#                        value = d[k] if k in d else (k if k in d.values() else None)
#                    case Callable() as f:
#                        value = f(column_value) if column_value else None
#                    case str() as name:
#                        writer = mappings.get(name)
#                        if name == PARAMETERIZED:
#                            value = writer(column_value, arg_dict) if column_value else None
#                        else:
#                            value = writer(column_value) if writer else None
#                    case _:
#                        value = None
#                option_values[column] = value # add value to dict
#                if column == SHORT_NAME:
#                    option_values[MANGLED_NAME] = mappings[MANGLED](column_value)
#                    option_values[INTERNAL_NAME] = mappings[INTERNAL_NAME](column_value)
#                elif column == DIMS:
#                    dims = value
#                elif column == UNGRIDDED_DIMS:
#                    ungridded = value
#                elif column == ALIAS:
#                    alias = value
            for key in set(options.keys()).difference(option_values.keys()):
                option = options[key]
                if FROM in option:
                    from_keys = tuple_wrapper(option[FROM])
                    from_values = list(map(lambda fk: spec[fk] if fk in spec else option_values.get(fk, None), 
                        from_keys))
                else:
                    from_values = [spec.get(key, None)]
#                    from_values = [option_values.get(from_key) for from_key in tuple_wrapper(option[FROM])]
                mapping = option.get(MAPPING, IDENTITY) 
                option_values[key] = mapping(*from_values)
# MANDATORY
            for option in mandatory_options:
                if option not in option_values:
                    raise RuntimeError(option + " is missing from spec.")
# END MANDATORY
#            option_values[RANK] = compute_rank(dims, ungridded)

# CHECKS HERE (Temporarily disabled for MAPL3 fixme)
#            try:
#                check_option_values(option_values)
#            except Exception:
#                raise
# END CHECKS
            category_specs.append(option_values)
        digested_specs[state_intent] = category_specs 

    return digested_specs
"""    

# DIGEST SPECS            
"""
def xigest(specs_in, options, keys, mappings, global_values):

    def process_option(name, spec, values):

        def get_from_values(option, name, spec, values, global_values):

            def get_value(key):
                if key in spec:
                    rval = spec[key]
                if key != name and key in values:
                    rval = values[key]
                rval = global_values.get(key)
                return rval

            match option:
                case str() as s:
                    raise RuntimeError(f'Option is an alias: {s}')
                case dict() as d:
                    match d.get(FROM, name):
                        case str() as key:
                            val = get_value(key)
                        case tuple():
                            val = tuple(get_value(key) for key in keys)
                    if val is None:
                        raise RuntimeError('Unable to find value to map')
                    return val
                case _:
                    raise RuntimeError('Option is not a supported type')
        #END get_from_values

        def get_mapping_function(option):

            def inner(mapping, n):
                match mapping:
                    case str() as fname if n > 0 and fname in mappings:
                        return inner(mappings[fname], n-1)
                    case dict() as d:
                        return lambda v: d[v] if (v in d) else (v if (v in d.values()) else None)
                    case Callable() as f:
                        return f
                    case _:
                        raise RuntimeError('Unable to get mapping.')

            if option is None:
                raise RuntimeError('Option is None. Cannot find mapping.')
            m = option.get(MAPPING)
            if m:
                return inner(m, n=3)
            return lambda v: v
        #END get_mapping_function

        option = options.get(name)
        if option is None:
            raise RuntimeError('Option not found')
        match option:
            case dict():
                from_values = get_from_values(option, name, spec, values, global_values)
                mapping_function = get_mapping_function(option)
            case _:
                raise RuntimeError('Option is not a supported type.')
        if from_values is None:
            raise RuntimeError('Unable to find values to map from.')
        if mapping_function is None:
            raise RuntimeError('Unable to find mapping function.')
        name_out = option.get(AS, name)
        match from_values:
            case str():
                return {name_out: mapping_function(from_values)}
            case tuple(): 
                return {name_out: mapping_function(*from_values)}
            case _:
                raise RuntimeError('Type of values to map from is not supported.')
#                return {name_out: mapping_function(from_values)}

    # END process_option

    def get_option_name(name, options, level=1):
        match options.get(name):
            case str() as s:
                return s
            case dict():
                return name

    match specs_in:
        case dict() as d:
            spec_list = [x for xs in d.values() for x in xs]
        case list() as el:
            spec_list = specs_in
        case _:
            raise RuntimeError('Unsupported specs format')
    specs = (((get_option_name(k, options), v) for (k, v) in spec) for spec in spec_list)
#    for spec in spec_list:
#        s = {}
#        for key in spec:
#            v = spec[key]
#            k = get_option_name(key, options)
#            s[k] = v
#        specs += s

    all_values = []
    for n, spec in enumerate(specs):
        values = {}
        for k in keys:
            kk, v = process_option(k, spec, values)
            values[kk] = v
        missing = list(filter(lambda o: o not in values, get_mandatory_options(options)))
        if missing:
            raise RuntimeError(f"These options are missing for spec {n}: {', '.join(missing)}")
        
        all_values.append(values)

    return all_values
"""

#############################################
# MAIN program begins here
#############################################

if __name__ == "__main__":
    main()
# FIN
    sys.exit(SUCCESS)

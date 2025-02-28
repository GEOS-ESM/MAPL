#!/usr/bin/env python3
import argparse
import sys
import os
import csv
from collections import namedtuple
import operator
from functools import partial

from enum import Enum

#################################### ENUMS #####################################
INTENT = Enum('INTENT', 'IMPORT EXPORT INTERNAL')


################################# CONSTANTS ####################################
SUCCESS = 0
LONGNAME_GLOB_PREFIX = "longname_glob_prefix"
# constants for logicals
TRUE_VALUE = '.true.'
FALSE_VALUE = '.false.'
TRUE_VALUES = {'t', 'true', 'yes', 'y', 'si', 'oui', 'sim'}
FALSE_VALUES = {'f', 'false', 'no', 'n', 'no', 'non', 'nao'}

# constants used for DIMS and computing rank
DIMS_OPTIONS = [('MAPL_DimsVertOnly', 1, 'z'), ('MAPL_DimsHorzOnly', 2, 'xy'), ('MAPL_DimsHorzVert', 3, 'xyz')]
RANKS = dict([(entry, rank) for entry, rank, _ in DIMS_OPTIONS])


############################### HELPER FUNCTIONS ###############################
rm_quotes = lambda s: s.__str__().strip().strip('"\'').strip()
add_quotes = lambda s: "'" + s.__str__() + "'"
mk_array = lambda s: '[ ' + s.__str__() + ']'

def make_string_array(s):
    """ Returns a string representing a Fortran character array """
    if ',' in ss:
        ls = [s.strip() for s in s.strip().split(',')]
    else:
        ls = s.strip().split()
    ls = [rm_quotes(s) for s in ls if s]
    n = max(list(map(len, ls)))
    ss = ','.join([add_quotes(s) for s in ls])
    return f"[character(len={n}) :: {ss}]"

def make_entry_writer(dictionary):
    """ Returns a writer function that looks up the value in dictionary """
    return lambda key: dictionary[key] if key in dictionary else None

def mangle_name_prefix(name, parameters = None):
    pre = 'comp_name'
    if isinstance(parameters, tuple):
        pre = parameters[0] if parameters[0] else pre
    codestring = f"'//trim({pre})//'" 
    return string_writer(name.replace("*",codestring)) if name else None

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
    extra_rank = len(ungridded.strip('][').split(',')) if ungridded else 0
    return RANKS[dims] + extra_rank

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


######################### WRITERS for writing AddSpecs #########################
# Return the value
identity_writer = lambda value: value
# Return value in quotes
string_writer = lambda value: add_quotes(value) if value else None
# Return value in brackets
array_writer = lambda value: mk_array(value) if value else None
# Strip '.' and ' ' [SPACE]
lstripped = lambda s: s.lower().strip(' .')
# writer for character arrays
string_array_writer = lambda value: make_string_array(value) if value else None
# mangle name for SHORT_NAME
mangle_name = lambda name: string_writer(name.replace("*","'//trim(comp_name)//'")) if name else None 
# mangle name for internal use
make_internal_name = lambda name: name.replace('*','') if name else None
# writer for LONG_NAME
mangle_longname = ParameterizedWriter(mangle_name_prefix, LONGNAME_GLOB_PREFIX)
# writer for DIMS
DIMS_EMIT = make_entry_writer(dict([(alias, entry) for entry, _, alias in DIMS_OPTIONS]))
# writer for VLOCATION
VLOCATION_EMIT = make_entry_writer({'C': 'MAPL_VlocationCenter', 'E': 'MAPL_VlocationEdge', 'N': 'MAPL_VlocationNone'})
# writer for ADD2EXPORT
ADD2EXPORT_EMIT = make_entry_writer({'T': '.true.', 'F': '.false.'})
# writer for logical-valued arguments
logical_writer = lambda s: TRUE_VALUE if lstripped(s) in TRUE_VALUES else FALSE_VALUE if lstripped(s) in FALSE_VALUES else None
# writer for RESTART
RESTART_EMIT = make_entry_writer({'OPT'  : 'MAPL_RestartOptional', 'SKIP' : 'MAPL_RestartSkip',
        'REQ'  : 'MAPL_RestartRequired', 'BOOT' : 'MAPL_RestartBoot',
        'SKIPI': 'MAPL_RestartSkipInitial'})


################################### OPTIONS ####################################
# parent class for class Option
# defines a few methods
class OptionType(Enum):
    def __init__(self, name_key, writer = None, mandatory = False, output = True):
        self.name_key = name_key
        self.writer = writer if writer else identity_writer
        self.mandatory = mandatory
        self.output = output

    def __call__(self, value):
        return self.writer(value)

    @classmethod
    def get_mandatory_options(cls):
        return list(filter(lambda m: m.mandatory, list(cls))) 

# class for the possible options in a spec
# uses functional API for creation of members (instances) with multiple word names
Option = Enum(value = 'Option', names = {
# MANDATORY
        'SHORT_NAME': ('short_name', mangle_name, True), #COMMON
        'NAME': ('short_name', mangle_name, True),
        'DIMS': ('dims', DIMS_EMIT, True), #COMMON
        'UNITS': ('units', string_writer, True), #COMMON
# OPTIONAL
        'AVERAGING_INTERVAL': ('averaging_interval',),
        'AVINT': ('averaging_interval',),
        'DATATYPE': ('datatype',),
        'DEFAULT': ('default',),
        'FIELD_TYPE': ('field_type',),
        'HALOWIDTH': ('halowidth',),
        'LONG_NAME': ('long_name', mangle_longname),
        'LONG NAME': ('long_name', mangle_longname),
        'NUM_SUBTILES': ('num_subtitles',),
        'NUMSUBS': ('num_subtitles',),
        'PRECISION': ('precision',),
        'PREC': ('precision',),
        'REFRESH_INTERVAL': ('refresh_interval',),
        'RESTART': ('restart', RESTART_EMIT),
        'ROTATION': ('rotation',),
        'STAGGERING': ('staggering',),
        'STANDARD_NAME': ('standard_name', mangle_longname), #EXPORT #INTERNAL
        'UNGRIDDED_DIMS': ('ungridded_dims', array_writer),
        'UNGRID': ('ungridded_dims', array_writer),
        'UNGRIDDED': ('ungridded_dims', array_writer),
        'VLOCATION': ('vlocation', VLOCATION_EMIT),
        'VLOC': ('vlocation', VLOCATION_EMIT),
# these are Options that are not output but used to write 
        'ALIAS': {'writer': identity_writer, 'mandatory': False, 'output': False},
        'CONDITION': ('condition', identity_writer, False, False),
        'COND': ('condition', identity_writer, False, False),
        'ALLOC': ('alloc', identity_writer, False, False),
        'MANGLED_NAME': ('mangled_name', mangle_name, False, False),
        'INTERNAL_NAME': ('internal_name', make_internal_name, False, False),
        'RANK': ('rank', None, False, False)
    }, type = OptionType)
 
COMMON = 'SHORT_NAME DIMS UNITS'.split()
INCLUDES = {
    INTENT.IMPORT: ('LONG_NAME AVERAGING_INTERVAL DATATYPE DEFAULT FIELD_TYPE ' +
       'HALOWIDTH NUM_SUBTILES PRECISION REFRESH_INTERVAL RESTART ' +
       'ROTATION STAGGERING UNGRIDDED_DIMS VLOCATION').split() + COMMON, 
    INTENT.EXPORT: ['STANDARD_NAME'] + COMMON,
    INTENT.INTERNAL: ['STANDARD_NAME'] + COMMON
}

###################### RULES to test conditions on Options #####################
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


###############################################################
# MAPL_DATASPEC class
class MAPL_DataSpec:
    """Declare and manipulate an import/export/internal specs for a
       MAPL Gridded component"""

    DELIMITER = ', '
    TERMINATOR = '_RC)'

    def __init__(self, state_intent, spec_values, indent=3):
        self.state_intent = state_intent
        self.indent = indent
        self.mangled_name = spec_values[Option.MANGLED_NAME]
        self.internal_name = spec_values[Option.INTERNAL_NAME]
        self.condition = spec_values.get(Option.CONDITION)
        self.spec_values = spec_values

    def newline(self):
        return "\n" + " "*self.indent

    def continue_line(self):
        return "&" + self.newline() + "& "

    def emit_specs(self):
        return self.emit_header() + self.emit_args() + self.emit_trailer(nullify=False)

    # Pointers must be declared regardless of COND status.  Deactivated
    # pointers should not be _referenced_ but such sections should still
    # compile, so we must declare the pointers
    def emit_declare_pointers(self):
        dimension = 'dimension(:' + ',:'*(self.spec_values[Option.RANK]-1) + ')'
        text = self.newline() + 'real'
        if Option.PRECISION in self.spec_values:
            kind = self.spec_values.get(Option.PRECISION)
            text = text + '(kind=' + str(kind) + ')'
        text = text +', pointer, ' + dimension + ' :: ' + self.internal_name
        return text

    def emit_get_pointers(self):
        """ Generate MAPL_GetPointer calls for the MAPL_DataSpec (self) """
        """ Creates string by joining list of generated and literal strings """
        """ including if block (emit_header) and 'alloc = value' (emit_pointer_alloc) """
        return MAPL_DataSpec.DELIMITER.join(
            [ self.emit_header() + "call MAPL_GetPointer(" + self.state_intent.name,
              self.internal_name, self.mangled_name] + self.emit_pointer_alloc() +
            [ MAPL_DataSpec.TERMINATOR + self.emit_trailer(nullify=True) ] )

    def emit_pointer_alloc(self):
        EMPTY_LIST = []
        key = Option.ALLOC
        value = self.spec_values.get(key)
        if value:
            value = value.strip().lower()
            listout = [ key.name_key + '=' + get_fortran_logical(value) ] if len(value) > 0 else EMPTY_LIST
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
        text = "call MAPL_Add" + self.state_intent.name.capitalize() + "Spec(gc," + self.continue_line()
        for option in self.spec_values: #wdb idea deleteme reduce?
            if option.output:
                text = text + self.emit_arg(option)
        text = text + MAPL_DataSpec.TERMINATOR + self.newline()
        self.indent = self.indent - 5
        return text

    def emit_arg(self, option):
        value = self.spec_values.get(option)
        if value:
            text = option.name_key + "=" + value + MAPL_DataSpec.DELIMITER + self.continue_line()
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
    parser = argparse.ArgumentParser(description='Generate import/export/internal specs for MAPL Gridded Component')
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
                state_intent = INTENT[next(gen)[0].split()[1]]
                columns = [c.strip().upper() for c in next(gen)]
                specs[state_intent] = dataframe(gen, columns)
            except StopIteration:
                break

    return specs


# DIGEST
def digest(specs, args):
    """ Set Option values from parsed specs """
    arg_dict = vars(args)
    mandatory_options = Option.get_mandatory_options()
    digested_specs = dict()

    for state_intent in specs:
        category_specs = list() # All the specs for the state_intent
        for spec in specs[state_intent]: # spec from list
            dims = None
            ungridded = None
            alias = None
            option_values = dict() # dict of option values
            for column in spec: # for spec writer value
                column_value = spec[column]
                option = Option[column.upper()] # use column name to find Option
                 # writer value
                if type(option.writer) is ParameterizedWriter:
                    option_value = option.writer(column_value, arg_dict)
                else:
                    option_value = option.writer(column_value)
                option_values[option] = option_value # add value to dict
                if option == Option.SHORT_NAME:
                    option_values[Option.MANGLED_NAME] = Option.MANGLED_NAME(column_value)
                    option_values[Option.INTERNAL_NAME] = Option.INTERNAL_NAME(column_value)
                elif option == Option.DIMS:
                    dims = option_value
                elif option == Option.UNGRIDDED:
                    ungridded = option_value
                elif option == Option.ALIAS:
                    alias = option_value
            if alias:
                option_values[Option.INTERNAL_NAME] = alias
# MANDATORY
            for option in mandatory_options:
                if option not in option_values:
                    raise RuntimeError(option.name + " is missing from spec.")
# END MANDATORY
            option_values[Option.RANK] = compute_rank(dims, ungridded)
# CHECKS HERE (Temporarily disabled for MAPL3 fixme)
#            try:
#                check_option_values(option_values)
#            except Exception:
#                raise
# END CHECKS
            category_specs.append(option_values)
        digested_specs[state_intent] = category_specs 

    return digested_specs
    

################################# EMIT_VALUES ##################################
def emit_values(specs, args):

    if args.name:
        component = args.name
    else:
        component = os.path.splitext(os.path.basename(args.input))[0]
        component = component.replace('_Registry','')
        component = component.replace('_StateSpecs','')

# open all output files
    f_specs = {}
    for state_intent in INTENT:
        option = args.__dict__[state_intent.name.lower()+"_specs"]
        if option:
            fname = option.format(component=component)
            f_specs[state_intent] = open_with_header(fname)
        else:
            f_specs[state_intent] = None

    if args.declare_pointers:
        f_declare_pointers = open_with_header(args.declare_pointers.format(component=component))
    else:
        f_declare_pointers = None
    if args.get_pointers:
        f_get_pointers = open_with_header(args.get_pointers.format(component=component))
    else:
        f_get_pointers = None

# Generate code from specs (processed above)
    for state_intent in INTENT:
        if state_intent in specs:
            for spec_values in specs[state_intent]:
                spec = MAPL_DataSpec(state_intent, spec_values)
                if f_specs[state_intent]:
                    f_specs[state_intent].write(spec.emit_specs())
                if f_declare_pointers:
                    f_declare_pointers.write(spec.emit_declare_pointers())
                if f_get_pointers:
                    f_get_pointers.write(spec.emit_get_pointers())

# Close output files
    for f in list(f_specs.values()):
        if f:
            f.close()
    if f_declare_pointers:
        f_declare_pointers.close()
    if f_get_pointers:
        f_get_pointers.close()


#############################################
# MAIN program begins here
#############################################

if __name__ == "__main__":
# Process command line arguments
    args = get_args()

# Process blocked CSV input file
    parsed_specs = read_specs(args.input)

# Digest specs from file to output structure
    try:
        specs = digest(parsed_specs, args)

    except Exception:
        raise

# Emit values
    emit_values(specs, args)

# FIN
    sys.exit(SUCCESS)

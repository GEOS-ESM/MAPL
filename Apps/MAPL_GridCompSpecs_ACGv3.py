#!/usr/bin/env python3
import argparse
import sys
import os
import csv
from collections import namedtuple
from collections.abc import Callable
import operator
from functools import partial
from enum import IntFlag

OptionFlag = IntFlag('OptionFlag', 'ARGUMENT CONTROL GLOBAL CALCULATION MANDATORY PRINTABLE'.split())

################################# CONSTANTS ####################################
SUCCESS = 0
NL = "\n"
DELIMITER = ', '
TERMINATOR = '_RC)'
# keys for options
STATE_INTENT_KEY = 'state_intent'
# command-line option constants
LONGNAME_GLOB_PREFIX = "longname_glob_prefix"
# procedure names
ADDSPEC = "MAPL_GridCompAddFieldSpec"
GETPOINTER = "MAPL_GetPointer"
# Fortran keywords
CALL = 'call'
# constants for logicals
TRUE_VALUE = '.true.'
FALSE_VALUE = '.false.'
TRUE_VALUES = {'t', 'true', 'yes', 'y', 'si', 'oui', 'sim'}
FALSE_VALUES = {'f', 'false', 'no', 'n', 'no', 'non', 'nao'}

# lookup for computing rank

############################### HELPER FUNCTIONS ###############################
add_quotes = lambda s: "'" + str(s) + "'"
mk_array = lambda s: '[ ' + str(s).strip().strip('[]') + ']'

def mangle_name_prefix(name, parameters = None):
    pre = 'comp_name'
    if isinstance(parameters, tuple):
        pre = parameters[0] if parameters[0] else pre
    codestring = f"'//trim({pre})//'" 
    return writers['string'](name.replace("*",codestring)) if name else None

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
writers = {
    'string': lambda value: add_quotes(value),
    'array': lambda value: mk_array(value),
    'mangled': lambda name: add_quotes(name.replace("*","'//trim(comp_name)//'")),
    'internal_name': lambda name: name.replace('*',''),
    'parameterized': ParameterizedWriter(mangle_name_prefix, LONGNAME_GLOB_PREFIX)
}

make_flag_check = lambda FN: lambda o: int(o['flag'] & OptionFlag[F]) if 'flag' in o else 0
is_mandatory = make_flag_check('MANDATORY')
is_argument = make_flag_check('ARGUMENT')
is_printable = make_flag_check('PRINTABLE')
is_global =  make_flag_check('GLOBAL')
is_control = make_flag_check('CONTROL')
is_calculation = make_flag_check('CALCULATION')

PRINTABLE_ARGUMENT = OptionFlag.ARGUMENT | OptionFlag.PRINTABLE
MANDATORY_ARGUMENT = PRINTABLE_ARGUMENT | OptionFlag.MANDATORY
CONTROL = OptionFlag.CONTROL
CALCULATION = OptionFlag.CALCULATION

# dict for the possible options in a spec
OPTIONS = {    
# MANDATORY
    'dims': {'mandatory': True, 'flags': MANDATORY_ARGUMENT, 'writer': {
        'z': "'z'",
        'xy': "'xy'",
        'xyz': "'xyz'",
        'MAPL_DimsVertOnly': "'z'",
        'MAPL_DimsHorzOnly': "'xy'",
        'MAPL_DimsHorzVert': "'xyz'"
    }},
    'short_name': {'writer': 'mangled', 'mandatory': True, 'flags': MANDATORY_ARGUMENT},
    'standard_name': {'writer': 'parameterized', 'mandatory': True, 'flags': MANDATORY_ARGUMENT},
    STATE_INTENT_KEY: {'mandatory': True, 'flags': MANDATORY_ARGUMENT, 'writer': {
        'import': 'ESMF_STATEINTENT_IMPORT',
        'export': 'ESMF_STATEINTENT_EXPORT',
        'internal': 'ESMF_STATEINTENT_INTERNAL'
    }},
    'units': {'writer': 'string', 'mandatory': True, 'flags': MANDATORY_ARGUMENT},
# OPTIONAL
    'averaging_interval': {'flags': PRINTABLE_ARGUMENT, },
    'datatype': {'flags': PRINTABLE_ARGUMENT, },
    'default': {'flags': PRINTABLE_ARGUMENT, },
    'field_type': {'flags': PRINTABLE_ARGUMENT, },
    'halowidth': {'flags': PRINTABLE_ARGUMENT, },
    'num_subtiles': {'flags': PRINTABLE_ARGUMENT, },
    'precision': {'flags': PRINTABLE_ARGUMENT, },
    'refresh_interval': {'flags': PRINTABLE_ARGUMENT, },
    'restart': {'flags': PRINTABLE_ARGUMENT, 'writer': {
        'OPT'  : 'MAPL_RestartOptional',
        'SKIP' : 'MAPL_RestartSkip',
        'REQ'  : 'MAPL_RestartRequired',
        'BOOT' : 'MAPL_RestartBoot',
        'SKIPI': 'MAPL_RestartSkipInitial'
    }},
    'rotation': {'flags': PRINTABLE_ARGUMENT, },
    'staggering': {'flags': PRINTABLE_ARGUMENT, },
    'ungridded_dims': {'flags': PRINTABLE_ARGUMENT, 'writer': 'array'},
    'vstagger': {'flags': PRINTABLE_ARGUMENT, 'writer': {
         'C': 'VERTICAL_SCATTER_CENTER',
         'E': 'VERTICAL_SCATTER_EDGE',
         'N': 'VERTICAL_SCATTER_NONE',
    }},
# these are options that are not output but used to write 
    'alias': {'flags': CALCULATION, 'output': False},
    'condition': {'flags': CONTROL, 'output': False},
    'alloc': {'flags': CALCULATION, 'output': False},
    'mangled_name': {'flags': CALCULATION, 'writer': 'mangled', 'output': False},
    'internal_name': {'flags': CALCULATION, 'writer': 'make_internal_name', 'output': False},
    'rank': {'flags': CALCULATION, 'output': False},
# aliases
    'avint': 'averaging_interval',
    'cond': 'condition',
    'long name': 'standard_name',
    'long_name': 'standard_name',
    'name': 'short_name',
    'numsubs': 'num_subtiles',
    'prec': 'precision',
    'ungrid': 'ungridded_dims',
    'ungridded': 'ungridded_dims',
    'vloc': 'vstagger',
    'vlocation': 'vstagger',
}
 
make_flag_check = lambda FN: lambda o: int(o['flag'] & OptionFlag[F]) if 'flag' in o else 0
mandatory_option = make_flag_check('MANDATORY')
argument_option = make_flag_check('ARGUMENT')
printable_option = make_flag_check('PRINTABLE')
global_option =  make_flag_check('GLOBAL')
control_option = make_flag_check('CONTROL')
calculation_option = make_flag_check('CALCULATION')

def is_mandatory(option):
    rv = isinstance(option, dict)
    if(rv):
        rv = option.get('mandatory', False)
    return rv

def get_mandatory_options(options):
    return [name for name, value in options.items() if is_mandatory(value)]

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

    TERMINATOR = '_RC)'

    def __init__(self, spec_values, options, indent=3):
        self.spec_values = spec_values
        self.options = options
        self.indent = indent
        self.mangled_name = spec_values['mangled_name']
        self.internal_name = spec_values['internal_name']
        self.condition = spec_values.get('condition')
        self.state_intent = spec_values[STATE_INTENT_KEY]

    def newline(self, indent=True):
        return NL + (" "*self.indent if indent else "")

    def continue_line(self):
        return "&" + self.newline() + "& "

    def emit_specs(self):
        return self.emit_header() + self.emit_args() + self.emit_trailer(nullify=False)

    # Pointers must be declared regardless of COND status.  Deactivated
    # pointers should not be _referenced_ but such sections should still
    # compile, so we must declare the pointers
    def emit_declare_pointers(self):
        spec_values = self.spec_values
        rank, precision = (spec_values['rank'], spec_values.get('precision', None))
        dimension = 'dimension(:' + ',:'*(rank-1) + ')'
        text = self.newline() + 'real'
        if precision:
            text = text + '(kind=' + str(precision) + ')'
        return text +', pointer, ' + dimension + ' :: ' + self.internal_name + self.newline()


    def emit_get_pointers(self):
        """ Generate MAPL_GetPointer calls for the MAPL_DataSpec (self) """
        """ Creates string by joining list of generated and literal strings """
        """ including if block (emit_header) and 'alloc = value' (emit_pointer_alloc) """
        return DELIMITER.join(
            [ self.emit_header() + f"{CALL} {GETPOINTER}(" + self.state_intent,
              self.internal_name, self.mangled_name] + self.emit_pointer_alloc() +
            [ TERMINATOR + self.emit_trailer(nullify=True) ] )

    def emit_pointer_alloc(self):
        EMPTY_LIST = []
        key = 'alloc'
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
        text = f"{CALL} {ADDSPEC}(gc,{self.continue_line()}"
        for column in self.spec_values:
            if self.options[column].get('output', True): #wdb idea deleteme reduce?
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
        if STATE_INTENT_KEY not in d:
            d[STATE_INTENT_KEY] = intent
        return d

#    set_op = lambda op, seq1, seq2: set(seq1).op(seq2)
#    merge_dicts = lambda r, d: d | dict((k, r[k] if r[k] else d[k])
#        for set_op(r.keys() & d.keys()))
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
                df = dataframe(gen, columns, (STATE_INTENT_KEY, intent))
                #merged = [merge(row, defaults) for row in df]
                specs[intent] = [add_state_intent(d, intent) for d in df]
            except StopIteration:
                break

    return specs


# DIGEST
def digest(parsed_specs, args, options):
    """ Set Option values from parsed specs """
    arg_dict = vars(args)
    mandatory_options = get_mandatory_options(options)
    digested_specs = dict()

    mangle_option = options['mangled_name']
    internal_option = options['internal_name']
    for state_intent in parsed_specs:
        category_specs = list() # All the specs for the state_intent
        for spec in parsed_specs[state_intent]: # spec from list
            dims = None
            ungridded = None
            alias = None
            option_values = dict() # dict of option values
            for column in spec: # for spec writer value
                column_value = spec[column]
                option = options[column]
                if isinstance(option, str):
                    column = option
                    option = options[column]
                match option.get('writer'):
                    case dict() as d:
                        k = column_value
                        value = d[k] if k in d else (k if k in d.values() else None)
                    case Callable() as f:
                        value = f(column_value) if column_value else None
                    case str() as name:
                        writer = writers.get(name)
                        if name == 'parameterized':
                            value = writer(column_value, arg_dict) if column_value else None
                        else:
                            value = writer(column_value) if writer else None
                    case _:
                        value = column_value
                option_values[column] = value # add value to dict
                if column == 'short_name':
                    option_values['mangled_name'] = writers['mangled'](column_value)
                    option_values['internal_name'] = writers['internal_name'](column_value)
                elif column == 'dims':
                    dims = value
                elif column == 'ungridded_dims':
                    ungridded = value
                elif column == 'alias':
                    alias = value
            if alias:
                option_values['internal_name'] = alias
# MANDATORY
            for option in mandatory_options:
                if option not in option_values:
                    raise RuntimeError(option + " is missing from spec.")
# END MANDATORY
            option_values['rank'] = compute_rank(dims, ungridded)
# CHECKS HERE (Temporarily disabled for MAPL3 fixme)
#            try:
#                check_option_values(option_values)
#            except Exception:
#                raise
# END CHECKS
            category_specs.append(option_values)
        digested_specs[state_intent] = category_specs 

    return digested_specs
    
add_newline = lambda s: f"{s.rstrip()}{NL}"

################################# EMIT_VALUES ##################################
def emit_values(specs, args, options):

    if args.name:
        component = args.name
    else:
        component, _ = os.path.splitext(os.path.basename(args.input))
        component = component.replace('_Registry','')
        component = component.replace('_StateSpecs','')

    STATEINTENT_WRITER = options[STATE_INTENT_KEY]['writer']

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

        if intent in specs:
            for spec_values in specs[intent]:
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

# Main Procedure (Added to facilitate testing.)
def main():
# Process command line arguments
    args = get_args()

# Process blocked CSV input file
    parsed_specs = read_specs(args.input)

# Digest specs from file to output structure
    try:
        specs = digest(parsed_specs, args, OPTIONS)

    except Exception:
        raise

# Emit values
    emit_values(specs, args, OPTIONS)

#############################################
# MAIN program begins here
#############################################

if __name__ == "__main__":
    main()
# FIN
    sys.exit(SUCCESS)

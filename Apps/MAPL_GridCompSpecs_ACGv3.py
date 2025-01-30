#!/usr/bin/env python3
import argparse
import sys
import os
import csv
from collections import namedtuple
import operator
from functools import partial

from enum import Enum

"""
ImportSpec:
      type (ESMF_GridComp)            , intent(inout)   :: gc # 
      character (len=*)               , intent(in)      :: short_name #QUOTED, #MANGLED, #INTERNAL
      character (len=*)  , optional   , intent(in)      :: long_name #QUOTED
      character (len=*)  , optional   , intent(in)      :: units
      integer            , optional   , intent(in)      :: dims
      integer            , optional   , intent(in)      :: datatype
      integer            , optional   , intent(in)      :: num_subtiles
      integer            , optional   , intent(in)      :: vlocation
      integer            , optional   , intent(in)      :: refresh_interval
      integer            , optional   , intent(in)      :: averaging_interval
      integer            , optional   , intent(in)      :: halowidth
      integer            , optional   , intent(in)      :: precision
      real               , optional   , intent(in)      :: default
      integer            , optional   , intent(in)      :: restart
      integer            , optional   , intent(in)      :: ungridded_dims(:) # ARRAY
      integer            , optional   , intent(in)      :: field_type
      integer            , optional   , intent(in)      :: staggering
      integer            , optional   , intent(in)      :: rotation
      integer            , optional   , intent(out)     :: rc # skip

ExportSpec:
      type(ESMF_GridComp), intent(inout) :: gridcomp #
      class(KeywordEnforcer), optional, intent(in) :: unusable #skip
      character(len=*), intent(in) :: short_name #QUOTED, #MANGLED, #INTERNAL
      character(len=*), intent(in) :: standard_name #QUOTED
      character(len=*), optional, intent(in) :: units 
      integer, optional, intent(out) :: rc # skip

InternalSpec:
      type(ESMF_GridComp), intent(inout) :: gridcomp #
      class(KeywordEnforcer), optional, intent(in) :: unusable #skip
      character(len=*), intent(in) :: short_name #QUOTED, #MANGLED, #INTERNAL
      character(len=*), intent(in) :: standard_name #QUOTED
      character(len=*), optional, intent(in) :: units 
      integer, optional, intent(out) :: rc #skip
"""
################################# CONSTANTS ####################################
SUCCESS = 0
CATEGORIES = ("IMPORT","EXPORT","INTERNAL")
LONGNAME_GLOB_PREFIX = "longname_glob_prefix"
# constants for logicals
TRUE_VALUE = '.true.'
FALSE_VALUE = '.false.'
TRUE_VALUES = {'t', 'true', 'yes', 'y', 'si', 'oui', 'sim'}
FALSE_VALUES = {'f', 'false', 'no', 'n', 'no', 'non', 'nao'}

# constants used for Option.DIMS and computing rank
DIMS_OPTIONS = [('MAPL_DimsVertOnly', 1, 'z'), ('MAPL_DimsHorzOnly', 2, 'xy'), ('MAPL_DimsHorzVert', 3, 'xyz')]
RANKS = dict([(entry, rank) for entry, rank, _ in DIMS_OPTIONS])


############################### HELPER FUNCTIONS ###############################
def make_string_array(s):
    """ Returns a string representing a Fortran character array """
    rm_quotes = lambda s: s.strip().strip('"\'').strip()
    add_quotes = lambda s: "'" + s + "'"
    ss = s.strip()
    if ',' in ss:
        ls = [s.strip() for s in s.strip().split(',')]
    else:
        ls = s.strip().split()
    ls = [rm_quotes(s) for s in ls]
    ls = [s for s in ls if s]
    n = max(ls)
    ss = ','.join([add_quotes(s) for s in ls])
    return f"[character(len={n}) :: {ss}]"

def make_entry_emit(dictionary):
    """ Returns a emit function that looks up the value in dictionary """
    return lambda key: dictionary[key] if key in dictionary else None

def mangle_name_prefix(name, parameters = None):
    pre = 'comp_name'
    if isinstance(parameters, tuple):
        pre = parameters[0] if parameters[0] else pre
    codestring = f"'//trim({pre})//'" 
    return string_emit(name.replace("*",codestring)) if name else None

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
class ParameterizedEmitFunction:

    def __init__(self, emit, *parameter_keys):
        self.emit = emit
        self.parameter_keys = parameter_keys
        
    def __call__(self, name, parameters):
        parameter_values = tuple(parameters.get(key) for key in self.parameter_keys)
        return self.emit(name, parameter_values)


##################### EMIT functions for writing AddSpecs ######################
# Return the value CORRESPONDS TO SCALAR
identity_emit = lambda value: value
# Return value in quotes # CORRESPONDS TO QUOTED
string_emit = lambda value: ("'" + value + "'") if value else None
# Return value in brackets # CORRESPONDS TO ARRAY
array_emit = lambda value: ('[' + value + ']') if value else None
# Strip '.' and ' ' [SPACE]
lstripped = lambda s: s.lower().strip(' .')
# emit function for character arrays # CORRESPONDS TO ARRAY OF QUOTED
string_array_emit = lambda value: make_string_array(value) if value else None
# mangle name for SHORT_NAME #???
mangle_name = lambda name: string_emit(name.replace("*","'//trim(comp_name)//'")) if name else None 
# mangle name for internal use
make_internal_name = lambda name: name.replace('*','') if name else None
# emit function for LONG_NAME
mangle_longname = ParameterizedEmitFunction(mangle_name_prefix, LONGNAME_GLOB_PREFIX)
# emit for function for DIMS
DIMS_EMIT = make_entry_emit(dict([(alias, entry) for entry, _, alias in DIMS_OPTIONS]))
# emit function for Option.VLOCATION
VLOCATION_EMIT = make_entry_emit({'C': 'MAPL_VlocationCenter', 'E': 'MAPL_VlocationEdge', 'N': 'MAPL_VlocationNone'})
# emit function for Option.ADD2EXPORT
ADD2EXPORT_EMIT = make_entry_emit({'T': '.true.', 'F': '.false.'})
# emit function for logical-valued options
logical_emit = lambda s: TRUE_VALUE if lstripped(s) in TRUE_VALUES else FALSE_VALUE if lstripped(s) in FALSE_VALUES else None
# emit function for Option.RESTART
RESTART_EMIT = make_entry_emit({'OPT'  : 'MAPL_RestartOptional', 'SKIP' : 'MAPL_RestartSkip',
        'REQ'  : 'MAPL_RestartRequired', 'BOOT' : 'MAPL_RestartBoot',
        'SKIPI': 'MAPL_RestartSkipInitial'})


################################### OPTIONS ####################################
# parent class for class Option
# defines a few methods
class OptionType(Enum):
    def __init__(self, name_key, emit = None, mandatory = False, output = True):
        self.name_key = name_key
        self.emit = emit if emit else identity_emit
        self.mandatory = mandatory
        self.output = output

    def __call__(self, value):
        return self.emit(value)

    @classmethod
    def get_mandatory_options(cls):
        return list(filter(lambda m: m.mandatory, list(cls))) 

# class for the possible options in a spec
# uses functional API for creation of members (instances) with multiple word names
Option = Enum(value = 'Option', names = {
# MANDATORY
        'SHORT_NAME': ('short_name', mangle_name, True),
        'NAME': ('short_name', mangle_name, True),
        'DIMS': ('dims', DIMS_EMIT, True),
        'LONG_NAME': ('long_name', mangle_longname, True),
        'LONG NAME': ('long_name', mangle_longname, True),
        'UNITS': ('units', string_emit, True),
# OPTIONAL
        'ADD2EXPORT': ('add2export', ADD2EXPORT_EMIT),
        'ADDEXP': ('add2export', ADD2EXPORT_EMIT),
        'ATTR_INAMES': ('attr_inames', array_emit),
        'ATTR_IVALUES': ('attr_ivalues', array_emit),
        'ATTR_RNAMES': ('attr_rnames', array_emit),
        'ATTR_RVALUES': ('attr_rvalues', array_emit),
        'AVERAGING_INTERVAL': ('averaging_interval',),
        'AVINT': ('averaging_interval',),
        'DATATYPE': ('datatype',),
        'DEFAULT': ('default',),
        'DEPENDS_ON_CHILDREN': ('depends_on_children', logical_emit),
        'DEPENDS_ON': ('depends_on', string_array_emit),
        'FIELD_TYPE': ('field_type',),
        'FRIENDLYTO': ('friendlyto', string_emit),
        'FRIEND2': ('friendlyto', string_emit),
        'HALOWIDTH': ('halowidth',),
        'NUM_SUBTILES': ('num_subtitles',),
        'NUMSUBS': ('num_subtitles',),
        'PRECISION': ('precision',),
        'PREC': ('precision',),
        'REFRESH_INTERVAL': ('refresh_interval',),
        'RESTART': ('restart', RESTART_EMIT),
        'ROTATION': ('rotation',),
        'STAGGERING': ('staggering',),
        'UNGRIDDED_DIMS': ('ungridded_dims', array_emit),
        'UNGRID': ('ungridded_dims', array_emit),
        'UNGRIDDED': ('ungridded_dims', array_emit),
        'UNGRIDDED_COORDS': ('ungridded_coords', array_emit),
        'UNGRIDDED_NAME': ('ungridded_name',),
        'UNGRIDDED_UNIT': ('ungridded_unit',),
        'VLOCATION': ('vlocation', VLOCATION_EMIT),
        'VLOC': ('vlocation', VLOCATION_EMIT),
# these are Options that are not output but used to write 
        'CONDITION': ('condition', identity_emit, False, False),
        'COND': ('condition', identity_emit, False, False),
        'ALLOC': ('alloc', identity_emit, False, False),
        'MANGLED_NAME': ('mangled_name', mangle_name, False, False),
        'INTERNAL_NAME': ('internal_name', make_internal_name, False, False),
        'RANK': ('rank', None, False, False)
    }, type = OptionType) 
 

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

    def __init__(self, category, spec_values, indent=3):
        self.category = category
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
            [ self.emit_header() + "call MAPL_GetPointer(" + self.category,
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
        text = "call MAPL_Add" + self.category.capitalize() + "Spec(gc," + self.continue_line()
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
    """Returns dict of (category: list of dict of (option name: option value) """
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
                category = next(gen)[0].split()[1]
                bare_columns = next(gen)
                bare_columns = [c.strip() for c in bare_columns] #wdb TODO DELETEME merge above and below
                columns = [c.upper() for c in bare_columns]
                specs[category] = dataframe(gen, columns)
            except StopIteration:
                break

    return specs


# DIGEST
def digest(specs, args):
    """ Set Option values from parsed specs """
    arg_dict = vars(args)
    mandatory_options = Option.get_mandatory_options()
    digested_specs = dict()

    for category in specs:
        category_specs = list() # All the specs for the category
        for spec in specs[category]: # spec from list
            dims = None
            ungridded = None
            option_values = dict() # dict of option values
            for column in spec: # for spec emit value
                column_value = spec[column]
                option = Option[column.upper()] # use column name to find Option
                 # emit value
                if type(option.emit) is ParameterizedEmitFunction:
                    option_value = option.emit(column_value, arg_dict)
                else:
                    option_value = option.emit(column_value)
                option_values[option] = option_value # add value to dict
                if option == Option.SHORT_NAME:
                    option_values[Option.MANGLED_NAME] = Option.MANGLED_NAME(column_value)
                    option_values[Option.INTERNAL_NAME] = Option.INTERNAL_NAME(column_value)
                elif option == Option.DIMS:
                    dims = option_value
                elif option == Option.UNGRIDDED:
                    ungridded = option_value
# MANDATORY
            for option in mandatory_options:
                if option not in option_values:
                    raise RuntimeError(option.name + " is missing from spec.")
# END MANDATORY
            option_values[Option.RANK] = compute_rank(dims, ungridded)
# CHECKS HERE
            try:
                check_option_values(option_values)
            except Exception:
                raise
# END CHECKS
            category_specs.append(option_values)
        digested_specs[category] = category_specs 

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
    for category in CATEGORIES:
        option = args.__dict__[category.lower()+"_specs"]
        if option:
            fname = option.format(component=component)
            f_specs[category] = open_with_header(fname)
        else:
            f_specs[category] = None

    if args.declare_pointers:
        f_declare_pointers = open_with_header(args.declare_pointers.format(component=component))
    else:
        f_declare_pointers = None
    if args.get_pointers:
        f_get_pointers = open_with_header(args.get_pointers.format(component=component))
    else:
        f_get_pointers = None

# Generate code from specs (processed above)
    for category in CATEGORIES:
        if category in specs:
            for spec_values in specs[category]:
                spec = MAPL_DataSpec(category.lower(), spec_values)
                if f_specs[category]:
                    f_specs[category].write(spec.emit_specs())
                if f_declare_pointers:
                    f_declare_pointers.write(spec.emit_declare_pointers())
                if f_get_pointers:
                    f_get_pointers.write(spec.emit_get_pointers())

# Close output files
    for category, f in list(f_specs.items()):
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
# Process command line arguments NO CHANGE
    args = get_args()

# Process blocked CSV input file NO CHANGE INITIALLY; NEED YAML EVENTUALLY
# PARSED_SPECS MAY CHANGE
    parsed_specs = read_specs(args.input)

# Digest specs from file to output structure DIGEST MAY CHANGE; SPECS MAY CHANGE
    try:
        specs = digest(parsed_specs, args)

    except Exception:
        raise

# Emit values INITIALLY EMIT_VALUES WILL NOT CHANGE EXCEPT SPECS AND INTERNALS
    emit_values(specs, args)

# FIN
    sys.exit(SUCCESS)

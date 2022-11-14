#!/usr/bin/env python3
import argparse
import sys
import os
import csv
from collections.abc import Sequence as sequence
from collections import namedtuple
from enum import Enum

SUCCESS = 0

CATEGORIES = ("IMPORT","EXPORT","INTERNAL")

FUNCTION_TYPE_NAME = 'function'

# Names of options
SHORT_NAME = 'short_name'
CONDITION = 'condition'
LONG_NAME = 'long_name'
PRECISION = 'precision'
UNITS = 'units'
DIMS = 'dims'
UNGRIDDED = 'ungridded_dims'
MANGLED_NAME = 'mangled_name'
INTERNAL_NAME = 'internal_name'
RANK = 'rank'
ALLOC = 'alloc'

Fields = namedtuple('Fields', 'emit mandatory extra')


class Emitter:
    identity = lambda a: a
    _emit = identity

    @property
    def emit(self):
        return self._emit
    
    @emit.setter
    def emit(self, emit_):
        self._emit = emit_

class Mandatory:
    MANDATORY = True
    OPTIONAL = False
    _mandatory = OPTIONAL

    @property
    def mandatory(self):
        return self._mandatory

    @property
    def optional(self):
        return not self._mandatory

    def set_mandatory(self):
        self._mandatory = MANDATORY

    def set_optional(self):
        self._mandatory = OPTIONAL

class Option(Fields, Enum):

    def __new__(cls, value, *args):
        emit, mandatory, extra = args
        obj = super().__new__(cls, emit, mandatory, extra)

    SHORT_NAME = 'SHORT_NAME', mangle_name, True, {'mangle_name': mangle_name}
    NAME = _make_alias('SHORT_NAME')
    LONG_NAME = 'LONG_NAME'  string_emit, True, None
    (LONG_NAME, 'long name', string_emit, True),
    (UNITS, string_emit, True),
    (DIMS, {'z'  : 'MAPL_DimsVertOnly', 'xy' : 'MAPL_DimsHorzOnly', 'xyz': 'MAPL_DimsHorzVert'}),
    ('vlocation', 'vloc', {'C': 'MAPL_VlocationCenter', 'E': 'MAPL_VlocationEdge', 'N': 'MAPL_VlocationNone'}),
    ('add2export', 'addexp',  {'T': '.true.', 'F': '.false.'}),
    ('restart', {'OPT'  : 'MAPL_RestartOptional', 'SKIP' : 'MAPL_RestartSkip',
                 'REQ'  : 'MAPL_RestartRequired', 'BOOT' : 'MAPL_RestartBoot',
                 'SKIPI': 'MAPL_RestartSkipInitial'}),
    (UNGRIDDED, {'ungridded', 'ungrid'}, array_emit),
    ('friendlyto', 'friend2', string_emit),
    (PRECISION, 'prec'),
    ('num_subtiles', 'numsubs'),
    ('averaging_interval', 'avint'),
    'refresh_interval',
    'halowidth',
    'default',
    'field_type',
    'staggering',
    'rotation',
    'datatype',
    'attr_inames',
    'att_rnames',
    'attr_ivalues',
    'attr_rvalues',
    'ungridded_name',
    'ungridded_unit',
    'ungridded_coords'
    def __init__(self, name, aliases = set(), emit = None, is_mandatory = None):
        self.name = name
        self.aliases = aliases
        self.emit = emit
        self.is_mandatory = is_mandatory

    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, value):
        self._name = value

    @property
    def aliases(self):
        return self._aliases

    @aliases.setter
    def aliases(self, value):
        if isinstance(value, str):
            al = {value}
        elif isinstance(value, sequence):
            al = set(value)
        elif isinstance(value, set):
            al = value
        else:
            al = set()
        al.add(self.name)
        self._aliases = al

    @property
    def emit(self):
        return self._emit

    @emit.setter
    def emit(self, value):
        if value is None:
            em = lambda x: x
        elif isinstance(value, dict):
            em = lambda x: value[k] if k in value else k
        else:
            em = value
        self._emit = em

    @property
    def mandatory(self):
        return self._is_mandatory

    @mandatory.setter
    def mandatory(self, value):
        self._is_mandatory = value

    @staticmethod
    def make(args):
        if isinstance(args, str):
            name = args
            remaining_args = ()
        elif isinstance(args, sequence):
            name = args[0]
            remaining_args = args[1:]
        else:
            raise Exception('Option.make illegal argument type: ' + type(args).__name__)
        aliases = set()
        emit = None
        is_mandatory = None
        for arg in remaining_args:
            if isinstance(arg, str):
                aliases.add(arg)
            elif isinstance(arg, set) or isinstance(arg, sequence):
                aliases.update(arg)
            elif isinstance(arg, dict) or type(arg).__name__ == FUNCTION_TYPE_NAME:
                if emit is None:
                    emit = arg
            elif isinstance(arg, bool):
                if is_mandatory is None:
                    is_mandatory = arg
        return Option(name, aliases, emit, is_mandatory)

    def __call__(self, value):
        return self.emit(value) if value else None


class NameOption(Option):

    def __init__(self):
        super().__init__(name = SHORT_NAME, aliases = 'name', emit = mangle_name, is_mandatory = True)

    @property
    def mangled_name(self):
        return self._mangled_name

    @mangled_name.setter
    def mangled_name(self, value):
        self._mangled_name = mangle_name(value)

    @property
    def internal_name(self):
        return self._internal_name

    @internal_name.setter
    def internal_name(self, value):
        self._internal_name = make_internal_name(value)

    def __call__(self, value):
        if value:
            self.mangled_name = value
            self.internal_name = value
        super().__call__(value) 

    def make(name):
        return NameOption()


string_emit = lambda value: "'" + value + "'" if value else None
array_emit = lambda value: '[' + value + ']' if value else None
mangle_name = lambda name: string_emit(name.replace("*","'//trim(comp_name)//'")) if value else None 
make_internal_name = lambda name: name.replace('*','') if value else None

def parse_spec(spec):
    name = spec.get(SHORT_NAME)
    if name:
        mangled_name = mangle_name(name)
        internal_name = make_internal_name(name)
    else:
        raise Exception(SHORT_NAME + " is mandatory.")

    condition = spec.get(CONDITION)
    precision = spec.get(PRECISION)
    dims = spec.get(DIMS)
    ungridded = spec.get(UNGRIDDED)
    rank = compute_rank(self.dims, self.ungridded)

ranks = {'MAPL_DimsHorzVert':3, 'MAPL_DimsHorzOnly':2, 'MAPL_DimsVertOnly':1} #wdb deleteme this should not a second constant

options = map(Option.make, (
#    (SHORT_NAME, 'name', mangle_name, True),
    (LONG_NAME, 'long name', string_emit, True),
    (UNITS, string_emit, True),
    (DIMS, {'z'  : 'MAPL_DimsVertOnly', 'xy' : 'MAPL_DimsHorzOnly', 'xyz': 'MAPL_DimsHorzVert'}),
    ('vlocation', 'vloc', {'C': 'MAPL_VlocationCenter', 'E': 'MAPL_VlocationEdge', 'N': 'MAPL_VlocationNone'}),
    ('add2export', 'addexp',  {'T': '.true.', 'F': '.false.'}),
    ('restart', {'OPT'  : 'MAPL_RestartOptional', 'SKIP' : 'MAPL_RestartSkip',
                 'REQ'  : 'MAPL_RestartRequired', 'BOOT' : 'MAPL_RestartBoot',
                 'SKIPI': 'MAPL_RestartSkipInitial'}),
    (UNGRIDDED, {'ungridded', 'ungrid'}, array_emit),
    ('friendlyto', 'friend2', string_emit),
    (PRECISION, 'prec'),
    ('num_subtiles', 'numsubs'),
    ('averaging_interval', 'avint'),
    'refresh_interval',
    'halowidth',
    'default',
    'field_type',
    'staggering',
    'rotation',
    'datatype',
    'attr_inames',
    'att_rnames',
    'attr_ivalues',
    'attr_rvalues',
    'ungridded_name',
    'ungridded_unit',
    'ungridded_coords'
))

#make_aliases
def make_aliases(option):
    result = (option.aliases, option.name)
    return result

def compute_rank(dims, ungridded):
    extra_rank = len(ungridded.strip('][').split(',')) if ungridded else 0
    return ranks[dims] + extra_rank

def zipdict(*dicts, fillvalue = None):
    zd = lambda dicts_, fillvalue_: {k: tuple(d.get(k, fillvalue_) for d in dicts_) for k in dicts_[0].keys()}
    return zd(dicts, fillvalue) if dicts else None

option_dict = dict(((option.name, option) for option in options))
option_dict[SHORT_NAME] = NameOption()

#aliases_name_tuples = (make_aliases(option) for option in options)
alias_name_tuples = list()
for option_name in option_dict:
    option = option_dict[option_name]
    pair = make_aliases(option)
    aliases, name = pair
    for alias in aliases:
        t = (alias, name)
        alias_name_tuples.append(t)
column_names = dict(alias_name_tuples)
column_names['cond'] = CONDITION
column_names[ALLOC] = ALLOC

#alias_name_tuples=((alias, name) for alias, name in aliases_name_tuples)

#for option in options:
#    for alias in option.aliases:
#        column_names[alias] = name

###############################################################
class MAPL_DataSpec:
    """Declare and manipulate an import/export/internal specs for a
       MAPL Gridded component"""

    DELIMITER = ', '
    TERMINATOR = '_RC)'

    def __init__(self, category, args,  option_dict, indent=3):
        self.category = category
        self.args = args
        self.option_dict = option_dict
        self.indent = indent

    def get_args(self):
        return self.args

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
        dimension = 'dimension(:' + ',:'*(self.rank-1) + ')'
        text = self.newline() + 'real'
        if self.kind:
            text = text + '(kind=' + str(self.kind) + ')'
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
        key = MAPL_DataSpec.ALLOC
        value = self.args.get(key)
        if value:
            value = value.strip().lower()
            listout = [ key + '=' + get_fortran_logical(value) ] if len(value) > 0 else EMPTY_LIST
        else:
            listout = EMPTY_LIST
        return listout

    def emit_header(self):
        text = self.newline()
        condition = self.args.get(self.condition)
        if condition:
            self.indent = self.indent + 3
            text = text + "if (" + condition + ") then" + self.newline()
        return text

    def emit_args(self):
        self.indent = self.indent + 5
        text = "call MAPL_Add" + self.category.capitalize() + "Spec(gc," + self.continue_line()
        for option_name in self.option_dict: #wdb idea deleteme reduce?
            text = text + self.emit_arg(option_name)
        text = text + MAPL_DataSpec.TERMINATOR + self.newline()
        self.indent = self.indent - 5
        return text

    def emit_arg(self, option_name):
        value = self.args.get(option_name)
        if value is None:
            text = ''
        else:
            emit = self.option_dict[option_name].emit
            if value:
                text = option_name + "=" + emit(value) + MAPL_DataSpec.DELIMITER + self.continue_line()
            elif option.is_mandatory:
                raise Exception(option_name + ' must have a non-blank value.')
        return text

    def emit_trailer(self, nullify=False):
        condition = self.args.get(self.condition)
        if condition:
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


def read_specs(specs_filename, column_names):
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
                bare_columns = [c.strip() for c in bare_columns]
                columns = [column_names[c.lower()] for c in bare_columns]
                specs[category] = dataframe(gen, columns)
            except StopIteration:
                break

    return specs

def get_fortran_logical(value_in):
    """ Return string representing Fortran logical from an input string """
    """ representing a logical value into """
    TRUE_VALUE = '.true.'
    FALSE_VALUE = '.false.'
    TRUE_VALUES = {TRUE_VALUE, 't', 'true', '.t.', 'yes', 'y', 'si', 'oui', 'sim'}
    FALSE_VALUES = {FALSE_VALUE, 'f', 'false', '.f.', 'no', 'n', 'no', 'non', 'nao'}
    if value_in is None:
        sys.exit("'None' is not valid for get_fortran_logical.")
    if value_in.strip().lower() in TRUE_VALUES:
        val_out = TRUE_VALUE
    elif value_in.strip().lower() in FALSE_VALUES:
        val_out = FALSE_VALUE
    else:
        sys.exit("Unrecognized logical: " + value_in)
    return val_out

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

def digest(zipped_dict, category):
    result = dict()
    for option_name, (option, value) in zipped_dict.items():
        newvalue = option(value)
        if isinstance(option, NameOption):
            mangled_name = option.mangled_name
            result[MANGLED_NAME] = mangled_name
            internal_name = option.internald_name
            result[INTERNAL_NAME] = internal_name

    dims = result.get(DIMS)
    ungridded = result.get(UNGRIDDED)
    rank = compute_rank(self.dims, self.ungridded)
    result[RANK] = rank

    return result
    
#############################################
# Main program begins here
#############################################

if __name__ == "__main__":

# Process command line arguments
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
    args = parser.parse_args()

# Process blocked CSV input file
    specs = read_specs(args.input, column_names)

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
            for args in specs[category]:
                zipped = zipdict(option_dict, args)
                spec = MAPL_DataSpec(category.lower(), args, option_dict)
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

    sys.exit(SUCCESS)

#!/usr/bin/env python
import argparse
import sys
import os
import csv
import warnings

SUCCESS = 0

CATEGORIES = ("IMPORT","EXPORT","INTERNAL")

###############################################################
class MAPL_DataSpec:
    """Declare and manipulate an import/export/internal specs for a 
       MAPL Gridded component"""

    all_options = ['short_name', 'long_name', 'units',
                   'dims', 'vlocation', 'num_subtiles',
                   'refresh_interval', 'averaging_interval', 'halowidth',
                   'precision','default','restart', 'ungridded_dims',
                   'field_type', 'staggering', 'rotation',
                   'friendlyto', 'add2export', 'datatype',
                   'attr_inames', 'att_rnames', 'attr_ivalues', 'attr_rvalues',
                   'ungridded_name', 'ungridded_unit', 'ungridded_coords']

    # The following arguments are skipped if value is empty string
    optional_options = [ 'dims', 'vlocation', 'num_subtiles',
                         'refresh_interval', 'averaging_interval', 'halowidth',
                         'precision','default','restart', 'ungridded_dims',
                         'field_type', 'staggering', 'rotation',
                         'friendlyto', 'add2export', 'datatype',
                         'attr_inames', 'att_rnames', 'attr_ivalues', 'attr_rvalues',
                         'ungridded_name', 'ungridded_unit', 'ungridded_coords']

    entry_aliases = {'dims': {'z'  : 'MAPL_DimsVertOnly',
                              'xy' : 'MAPL_DimsHorzOnly',
                              'xyz': 'MAPL_DimsHorzVert'},
                     'vlocation': {'C': 'MAPL_VlocationCenter',
                                   'E': 'MAPL_VlocationEdge',
                                   'N': 'MAPL_VlocationNone'},
                     'restart': {'OPT'  : 'MAPL_RestartOptional',
                                 'SKIP' : 'MAPL_RestartSkip',
                                 'REQ'  : 'MAPL_RestartRequired',
                                 'BOOT' : 'MAPL_RestartBoot',
                                 'SKIPI': 'MAPL_RestartSkipInitial'},
                     'add2export': {'T': '.true.', 'F': '.false.'}
                 }

    # The following options require quotes in generated code
    stringlike_options = ['short_name', 'long_name', 'units', 'friendlyto']


    # The following arguments must be placed within array brackets.
    arraylike_options = ['ungridded_dims']

    ALLOC = 'alloc'
    DELIMITER = ', '
    TERMINATOR = '_RC)'

    def __init__(self, category, args, indent=3):
        self.category = category
        self.args = args
        self.indent  = indent

    def newline(self):
        return "\n" + " "*self.indent

    def continue_line(self):
        return "&" + self.newline() + "& "

    def emit_specs(self):
        return self.emit_header() + self.emit_args() + self.emit_trailer(nullify=False)

    def get_rank(self):
        ranks = {'MAPL_DimsHorzVert':3, 'MAPL_DimsHorzOnly':2, 'MAPL_DimsVertOnly':1}
        extra_rank = 0 # unless
        if 'ungridded_dims' in self.args:
            ungridded = self.args['ungridded_dims']
            if ungridded:
                extra_dims = ungridded.strip('][').split(',')
                extra_rank = len(extra_dims)
        aliases = MAPL_DataSpec.entry_aliases['dims']
        dims = self.args['dims']
        if dims in aliases:
            dims = aliases[dims]
        return ranks[dims] + extra_rank

    @staticmethod
    def internal_name(name):
        return name.replace('*','')

    @staticmethod
    def mangled_name(name):
        return "'" + name.replace("*","'//trim(comp_name)//'") + "'"

    # Pointers must be declared regardless of COND status.  Deactivated
    # pointers should not be _referenced_ but such sections should still
    # compile, so we must declare the pointers
    def emit_declare_pointers(self):
        text = self.newline()
        type = 'real'
        if 'precision' in self.args:
            kind = self.args['precision']
        else:
            kind = None
        rank = self.get_rank()
        dimension = 'dimension(:' + ',:'*(rank-1) + ')'
        text = text + type
        if kind:
            text = text + '(kind=' + str(kind) + ')'
        text = text +', pointer, ' + dimension + ' :: ' + MAPL_DataSpec.internal_name(self.args['short_name'])
        return text

    def emit_get_pointers(self):
        """ Generate MAPL_GetPointer calls for the MAPL_DataSpec (self) """
        """ Creates string by joining list of generated and literal strings """
        """ including if block (emit_header) and 'alloc = value' (emit_pointer_alloc """
        return MAPL_DataSpec.DELIMITER.join(
            [   self.emit_header() + "call MAPL_GetPointer(" + self.category,
                MAPL_DataSpec.internal_name(self.args['short_name']),
                MAPL_DataSpec.mangled_name(self.args['short_name']) ] + 
            self.emit_pointer_alloc() +
            [   MAPL_DataSpec.TERMINATOR + self.emit_trailer(nullify=True) ] )

    def emit_pointer_alloc(self):
        EMPTY_LIST = []
        key = MAPL_DataSpec.ALLOC
        if key in self.args:
            value = self.args[key].strip().lower()
            listout = [ key + '=' + get_fortran_logical(value) ] if len(value) > 0 else EMPTY_LIST
        else:
            listout = EMPTY_LIST
        return listout

    def emit_header(self):
        text = self.newline()
        if 'condition' in self.args and self.args['condition']:
            self.indent = self.indent + 3
            text = text + "if (" + self.args['condition']  + ") then" + self.newline() 
        return text

    def emit_args(self):
        self.indent = self.indent + 5
        text = "call MAPL_Add" + self.category.capitalize() + "Spec(gc," + self.continue_line()
        for option in MAPL_DataSpec.all_options:
            text = text + self.emit_arg(option)
        text = text + MAPL_DataSpec.TERMINATOR + self.newline()
        self.indent = self.indent - 5
        return text

    def emit_arg(self, option):
        text = ''
        if option in self.args:
            value = self.args[option]
            if option in MAPL_DataSpec.optional_options:
                if self.args[option] == '':
                    return ''
            text = text + option + "="
            if option in MAPL_DataSpec.stringlike_options:
                if option == 'short_name':
                   value = MAPL_DataSpec.mangled_name(value)
                else: 
                   value = "'"+value+"'"
            elif option in MAPL_DataSpec.arraylike_options:
                value = '[' + value + ']' # convert to Fortran 1D array
            else:
                if (option in MAPL_DataSpec.entry_aliases) and (
                        self.args[option] in MAPL_DataSpec.entry_aliases[option]):
                    value = MAPL_DataSpec.entry_aliases[option][self.args[option]]
                else:
                    value = self.args[option]
            text = text + value + MAPL_DataSpec.DELIMITER + self.continue_line()
        return text

    def emit_trailer(self, nullify=False):
        if 'condition' in self.args and self.args['condition']:
            self.indent = self.indent - 3
            short_name = MAPL_DataSpec.internal_name(self.args['short_name'])
            text = self.newline()
            if nullify:
                text = text + "else" + self.newline()
                text = text + "   nullify(" + short_name + ")" + self.newline()
            text = text + "endif" + self.newline()
        else:
            text = self.newline()
        return text


def read_specs(specs_filename):
    
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
    # Aliases must be lowercase.
    column_aliases = {
        'name'       : 'short_name',
        'long name'  : 'long_name',
        'vloc'       : 'vlocation',
        'ungridded'  : 'ungridded_dims',
        'ungrid'     : 'ungridded_dims',
        'prec'       : 'precision',
        'cond'       : 'condition',
        'friend2'    : 'friendlyto',
        'addexp'     : 'add2export',
        'numsubs '   : 'num_subtiles,',
        'avint'      : 'averaging_interval'
    }

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
                columns = []
                for c in bare_columns:
                    columns.append(getifin(column_aliases, c))
                specs[category] = dataframe(gen, columns)
            except StopIteration:
                break

    return specs

def getifin(dictionary, key):
    """ Return dictionary[key.lower()] if key.lower() in dictionary else key """ 
    return dictionary[key.lower()] if key.lower() in dictionary else key.lower()

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

#############################################
# Main program begins here
#############################################


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
specs = read_specs(args.input)

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
        for item in specs[category]:
            spec = MAPL_DataSpec(category.lower(), item)
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

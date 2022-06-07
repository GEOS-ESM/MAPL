#!/usr/bin/env python
import argparse
import sys
import os
import csv

# This avoids repeating these strings in different places which leads to errors.
CONDITION = 'condition'
DIMS = 'dims'
DIMS_ENTRY_ALIASES = {'z': DIMS_VERT_ONLY, 'xy': DIMS_HORZ_ONLY, 'xyz': DIMS_HORZ_VERT}
DIMS_HORZ_ONLY = 'MAPL_DimsHorzOnly'
DIMS_HORZ_VERT = 'MAPL_DimsHorzVert'
DIMS_VERT_ONLY = 'MAPL_DimsVertOnly'
NAME = 'short_name'

def make_entry_parser(aliases):
    """ return entry_parser that looks up aliases for values aliases """

    def parser(value):
        """ return aliases[value] or value """
        if value in aliases:
            return aliases[value]
        else:
            return value

    return parser

def shortname_parser(value):
    """ return value mangled """
    return MAPL_DataSpec.mangled_name(value)
    
def stringlike_parser(value):
    """ return value in Fortran str syntax """
    return "'"+value+"'"
    
def arraylike_parser(value):
    """ return value in Fortran 1D array syntax """
    return '[' + value + ']'

def entry_parser(value):
    """ return value unaliased as a str from a set of values """
    return value

def null_parser(value):
    """ return None (do nothing parser) """
    return None

################################################################################

class SpecOpt:

    def __init__(self, name, parser = entry_parser, alias = None, optional = True):
        self.name = name
        self.alias = alias
        self.optional = optional

        if isinstance(parser, dict):
            self.parser = make_entry_parser(parser)
        elif isinstance(parser, list) and all(list(map(lambda t: isinstance(t, tuple), parser))) :
            self.parser = make_entry_parser(dict(parser))
        else:
            self.parser = parser

    @staticmethod
    def make_dict(specopt_list):
        t1 = list(map(lambda el: (el.name, el), specopt_list))
        t2 = list(map(lambda el: (el.alias, el), filter(lambda el: el.alias, specopt_list )))
        return dict(t1 + t2)

################################################################################

options = SpecOpt.make_dict([
    SpecOpt(NAME, parser = shortname_parser, alias = 'NAME', optional = False),
    SpecOpt('long_name', parser = stringlike_parser, alias = 'LONG NAME', optional = False),
    SpecOpt('units', parser = stringlike_parser, alias = 'UNITS', optional = False),
    SpecOpt(DIMS, parser = DIMS_ENTRY_ALIASES, alias = 'DIMS'),
    SpecOpt('vlocation', parser =
        {'C': 'MAPL_VlocationCenter', 'E': 'MAPL_VlocationEdge', 'N': 'MAPL_VlocationNone'},
        alias = 'VLOC'),
    SpecOpt('num_subtiles'),
    SpecOpt('refresh_interval'),
    SpecOpt('averaging_interval', alias = 'AVERAGING_INTERVAL'),
    SpecOpt('halowidth'),
    SpecOpt('precision', alias = 'PREC'),
    SpecOpt('default', alias = 'DEFAULT'),
    SpecOpt('restart', parser =
        {'OPT'  : 'MAPL_RestartOptional', 'SKIP' : 'MAPL_RestartSkip', 'REQ'  : 'MAPL_RestartRequired',
        'BOOT' : 'MAPL_RestartBoot', 'SKIPI': 'MAPL_RestartSkipInitial'}, 
        alias = 'RESTART'),
    SpecOpt('ungridded_dims', parser = arraylike_parser, alias = 'UNGRIDDED' ),
    SpecOpt('field_type'),
    SpecOpt('staggering'),
    SpecOpt('rotation'),
    SpecOpt('friendlyto', parser = stringlike_parser, alias = 'FRIENDLYTO'),
    SpecOpt('add2export', parser = {'T': '.true.', 'F': '.false.'},  alias = 'ADD2EXPORT'),
    SpecOpt('datatype', alias = 'DATATYPE'),
    SpecOpt('restart'),
    SpecOpt('attr_inames'),
    SpecOpt('att_rnames'),
    SpecOpt('attr_ivalues'),
    SpecOpt('attr_rvalues'),
    SpecOpt('ungridded_name'),
    SpecOpt('ungridded_unit'),
    SpecOpt('ungridded_coords'),
    # This is a special control option that does no parsing.
    SpecOpt(CONDITION, parser = null_parser, alias = 'COND')
    ])

# These may be used in the future. Leaving them here for now for information. 
#common_options =    [ NAME, 'long_name', 'units', DIMS, 'datatype', 
#                    'vlocation', 'num_subtiles', 'refresh_interval', 
#                    'averaging_interval', 'precision', 'default', 'halowidth', 
#                    'ungridded_dims', 'field_type', 'staggering', 'rotation' ]
#
#internal_options =  [ 'restart', 'friendlyto', 'add2export', 'attr_inames',
#    'attr_rnames', 'attr_ivalues', 'attr_rvalues' ]
#
#options_by_category = { 'IMPORT' : common_options + [ 'restart' ],
#    'EXPORT'   : common_options + [ 'ungridded_name', 'ungridded_unit', 'ungridded_coords' ],
#    'INTERNAL' : common_options + internal_options }
#
#categories = options_by_category.keys()
categories = ['IMPORT', 'EXPORT', 'INTERNAL']

column_aliases = dict(map(lambda x: (x.alias, x.name), filter(lambda x: x.alias, options.values())))
#column_aliases = dict(map(lambda x: (x, x.alias), filter(lambda x: x.alias, options.values() )))

###############################################################

class MAPL_DataSpec:
    """Declare and manipulate an import/export/internal specs for a 
       MAPL Gridded component"""

#    entry_aliases = {DIMS: {'z'  : DIMS_VERT_ONLY,
#                              'xy' : DIMS_HORZ_ONLY,
#                              'xyz': DIMS_HORZ_VERT},
#                     'vlocation': {'C': 'MAPL_VlocationCenter',
#                                   'E': 'MAPL_VlocationEdge',
#                                   'N': 'MAPL_VlocationNone'},
#                     'restart': {'OPT'  : 'MAPL_RestartOptional',
#                                 'SKIP' : 'MAPL_RestartSkip',
#                                 'REQ'  : 'MAPL_RestartRequired',
#                                 'BOOT' : 'MAPL_RestartBoot',
#                                 'SKIPI': 'MAPL_RestartSkipInitial'},
#                     'add2export': {'T': '.true.', 'F': '.false.'}
#                 }

    @staticmethod
    def dealias(args, aliases):
        """ Return a dict where keys are dealiased using a dict of aliases  """
        # if k in aliases, return k alias, otherwise return k - listwise
        return dict(
            map(
                lambda (key, value): (aliases[key] if key in aliases else key, value),
                    args.items()
            ))

    def __init__(self, category, args, indent=3):
        self.category = category
        self.args = MAPL_DataSpec.dealias(args, column_aliases)
        self.indent  = indent

    def newline(self):
        return "\n" + " "*self.indent

    def continue_line(self):
        return "&" + self.newline() + "& "

    @staticmethod
    def get_dict_value(dictionary, keyvalue):
        """
        Searches dictionary for keyvalue as key or value. Returns value.

        Throws KeyError exception if keyvalue is not found.
        """

        if keyvalue in dictionary.values():
            return keyvalue
        else:
            try:
                return dictionary[keyvalue]
            except KeyError as err:
                print(keyvalue + " was not found as key or value in dictionary.")
                raise

    def emit_specs(self):
        return self.emit_header() + self.emit_args() + self.emit_trailer(nullify=False)

    def get_rank(self):
        ranks = {DIMS_HORZ_VERT:3, DIMS_HORZ_ONLY:2, DIMS_VERT_ONLY:1}
        extra_rank = 0 # unless
        if 'ungridded_dims' in self.args:
            ungridded = self.args['ungridded_dims']
            if ungridded:
                extra_dims = ungridded.strip('][').split(',')
                extra_rank = len(extra_dims)

#        dims_aliases = MAPL_DataSpec.entry_aliases[DIMS]
        dims_aliases = DIMS_ENTRY_ALIASES

        try:
            dims = MAPL_DataSpec.get_dict_value(dims_aliases, self.args[DIMS])
        except KeyError as err:
            print(self.args)
            sys.exit('Unable to find alias. Exiting')

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
        TYPE_ = 'real'
        if 'precision' in self.args:
            kind = self.args['precision']
        else:
            kind = None
        rank = self.get_rank()
        dimension = 'dimension(:' + ',:'*(rank-1) + ')'
        text = text + TYPE_
        if kind:
            text = text + '(kind=' + str(kind) + ')'
        text = text +', pointer, ' + dimension + ' :: ' + MAPL_DataSpec.internal_name(self.args[NAME]) + ' => null()'
        text = text + self.newline()
        return text

    def emit_get_pointers(self):
        text = self.emit_header()
        short_name = MAPL_DataSpec.internal_name(self.args[NAME])
        mangled_name = MAPL_DataSpec.mangled_name(self.args[NAME])
        text = text + "call MAPL_GetPointer(" + self.category + ', ' + short_name + ", " + mangled_name + ", _RC)" 
        text = text + self.emit_trailer(nullify=True)
        return text

    def emit_header(self):
        text = self.newline()
        if CONDITION in self.args and self.args[CONDITION]:
            self.indent = self.indent + 3
            text = text + "if (" + self.args[CONDITION]  + ") then" + self.newline() 
        return text

    def emit_args(self):
        self.indent = self.indent + 5
        text = "call MAPL_Add" + self.category.capitalize() + "Spec(gc," + self.continue_line()
        for option in self.args:
            text = text + self.emit_arg(option)
        text = text + 'rc=status)' + self.newline()
        self.indent = self.indent - 5
        text = text + 'VERIFY_(status)'
        return text

    def emit_arg(self, option):
        text = ''
        spec_opt = options[option]
        if spec_opt.optional:
            if self.args[option] == '':
                return ''
        value = spec_opt.parser(self.args[option])
        if (value is None):
            return ''
        text = text + spec_opt.name + "="

        text = text + value + ", " + self.continue_line()
        return text

    def emit_trailer(self, nullify=False):
        if CONDITION in self.args and self.args[CONDITION]:
            self.indent = self.indent - 3
            short_name = MAPL_DataSpec.internal_name(self.args[NAME])
            text = self.newline()
            if nullify:
                text = text + "else" + self.newline()
                text = text + "   nullify(" + short_name + ")" + self.newline()
            text = text + "endif" + self.newline()
        else:
            text = self.newline()
        return text

###############################################################

def read_specs(specs_filename):
    """
    Read tab-delimited file of specs
    Returns a dict with categories as keys and lists of dict's
    as values. Each dict is a record with column names and values.
    """

    def csv_record_reader(csv_reader):
        """ Read a csv reader iterator until a blank line is found.
        Returns generator of list of fields"""
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
                columns = next(gen)
                columns = [c.strip() for c in columns]
                specs[category] = dataframe(gen, columns)
            except StopIteration:
                break

    return specs



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
                    help="override default output filename for AddSpec code")
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

for category in categories:
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
for category in categories:
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

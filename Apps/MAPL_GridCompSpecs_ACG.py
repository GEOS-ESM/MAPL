#!/usr/bin/env python
import argparse
import sys
import os
import csv
import pandas as pd


###############################################################
class MAPL_DataSpec:
    """Declare and manipulate an import/export/internal specs for a 
       MAPL Gridded component"""

    all_options = ['short_name', 'long_name', 'units',
                   'dims', 'vlocation', 'num_subtiles',
                   'refresh_interval', 'averaging_interval', 'halowidth',
                   'precision','default','restart', 'ungridded_dims',
                   'field_type', 'staggering', 'rotation',
                   'friendlyto', 'add2export']

    # The following arguments are skipped if value is empty string
    optional_options = [ 'dims', 'vlocation', 'num_subtiles',
                         'refresh_interval', 'averaging_interval', 'halowidth',
                         'precision','default','restart', 'ungridded_dims',
                         'field_type', 'staggering', 'rotation',
                         'friendlyto', 'add2export']

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


    def __init__(self, category, args, indent=3):
        self.category = category
        self.args = args
        self.indent  = indent

    def newline(self):
        return "\n" + " "*self.indent

    def continue_line(self):
        return "&" + self.newline() + "& "

    def emit_specs(self):
        return self.emit_header() + self.emit_args() + self.emit_trailer()

    def get_rank(self):
        ranks = {'MAPL_DimsHorzVert':3, 'MAPL_DimsHorzOnly':2, 'MAPL_DimsVertOnly':1}
        extra_rank = 0 # unless
        if 'ungridded_dims' in self.args:
            ungridded = self.args['ungridded_dims']
            if ungridded:
                extra_dims = ungridded.strip('][').split(',')
                extra_rank = len(extra_dims)
        dims = MAPL_DataSpec.entry_aliases['dims'][self.args['dims']]
        return ranks[dims] + extra_rank

    @staticmethod
    def internal_name(name):
        if name[-1] == '*':
            return name[:-1]
        else:
            return name
    @staticmethod
    def mangled_name(name):
        if name[-1] == '*':
            return "'" + name[:-1] + "'//trim(comp_name)"
        else:
            return "'" + name + "'"

    def emit_declare_pointers(self):
        text = self.emit_header()
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
        text = text +', pointer, ' + dimension + ' :: ' + MAPL_DataSpec.internal_name(self.args['short_name']) + ' => null()'
        text = text + self.emit_trailer()
        return text

    def emit_get_pointers(self):
        text = self.emit_header()
        short_name = MAPL_DataSpec.internal_name(self.args['short_name'])
        mangled_name = MAPL_DataSpec.mangled_name(self.args['short_name'])
        text = text + "call MAPL_GetPointer(" + self.category + ', ' + short_name + ", " + mangled_name + ", rc=status); VERIFY_(status)" 
        text = text + self.emit_trailer()
        return text

    def emit_header(self):
        text = self.newline()
        if 'CONDITION' in self.args and self.args['CONDITION']:
            self.indent = self.indent + 3
            text = text + "if (" + self.args['CONDITION']  + ") then" + self.newline() 
        return text

    def emit_args(self):
        self.indent = self.indent + 5
        text = "call MAPL_Add" + self.category.capitalize() + "Spec(gc," + self.continue_line()
        for option in MAPL_DataSpec.all_options:
            text = text + self.emit_arg(option)
        text = text + 'rc=status)' + self.newline()
        self.indent = self.indent - 5
        text = text + 'VERIFY_(status)'
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
            text = text + value + ", " + self.continue_line()
        return text

    def emit_trailer(self):
        if 'CONDITION' in self.args and self.args['CONDITION']:
            self.indent = self.indent - 3
            text = self.newline()
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

    column_aliases = {
        'NAME'       : 'short_name',
        'LONG NAME'  : 'long_name',
        'VLOC'       : 'vlocation',
        'UNITS'      : 'units',
        'DIMS'       : 'dims',
        'UNGRIDDED'  : 'ungridded_dims',
        'PREC'       : 'precision',
        'COND'       : 'condition',
        'DEFAULT'    : 'default',
        'RESTART'    : 'restart',
        'FRIENDLYTO' : 'friendlyto',
        'ADD2EXPORT' : 'add2export'
    }

    specs = {}
    with open(specs_filename, 'r') as specs_file:
        specs_reader = csv.reader(specs_file, skipinitialspace=True,delimiter='|')
        gen = csv_record_reader(specs_reader)
        schema_version = next(gen)[0].split(' ')[1]
        component = next(gen)[0].split(' ')[1]
#        print("Generating specification code for component: ",component)
        while True:
            try:
                gen = csv_record_reader(specs_reader)
                category = next(gen)[0].split()[1]
                bare_columns = next(gen)
                bare_columns = [c.strip() for c in bare_columns]
                columns = []
                for c in bare_columns:
                    if c in column_aliases:
                        columns.append(column_aliases[c])
                    else:
                        columns.append(c)
                specs[category] = pd.DataFrame(gen, columns=columns)
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


# Process blocked CSV input file using pandas
specs = read_specs(args.input)

if args.name:
    component = args.name
else:
    component = os.path.splitext(os.path.basename(args.input))[0]
    component = component.replace('_Registry','')
    component = component.replace('_StateSpecs','')

# open all output files
f_specs = {}
for category in ("IMPORT","EXPORT","INTERNAL"):
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

# Generate code from specs (processed above with pandas)
for category in ("IMPORT","EXPORT","INTERNAL"):
    for item in specs[category].to_dict("records"):
        spec = MAPL_DataSpec(category.lower(), item)
        if f_specs[category]:
            f_specs[category].write(spec.emit_specs())
        if f_declare_pointers:
            f_declare_pointers.write(spec.emit_declare_pointers())
        if f_get_pointers:
            f_get_pointers.write(spec.emit_get_pointers())

# Close output files
for category, f in f_specs.items():
    if f:
        f.close()
if f_declare_pointers:
    f_declare_pointers.close()
if f_get_pointers:
    f_get_pointers.close()



            
            

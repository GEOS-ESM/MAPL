import argparse
import sys
import os

my_tool = os.path.dirname(os.path.abspath(__file__)) + '/src'
sys.path.append(my_tool)

import MAPL_DataSpec
import reader

# command line arguments
parser = argparse.ArgumentParser(description='Generate import/export/internal specs for MAPL Gridded Component')
parser.add_argument('-i','--input', action='store')
parser.add_argument('--add_specs', action='store', default='Spec.h')
parser.add_argument('--declare_pointers', action='store', default='DeclarePointer.h')
parser.add_argument('--get_pointers', action='store', default='GetPointer.h')
args = parser.parse_args()


def header():
    return """
!                          -------------------
!                          W  A  R  N  I  N  G
!                          -------------------
!
!   This code fragment is automatically generated by a MAPL_GridCompSpecs_ACG.
!   Please DO NOT edit it. Any modification made in here will be overwritten
!   next time this file is auto-generated. Instead, enter your additions
!   or deletions in the .rc file in the src tree.
!
    """

specs = reader.read_specs(args.input)

def open_with_header(filename):
    f = open(filename,'w')
    f.write(header())
    return f

f_specs = {}
for category in ('IMPORT','EXPORT','INTERNAL'):
    f_specs[category] = open_with_header(category.capitalize()+args.add_specs)
    
f_declare_pointers = open_with_header(args.declare_pointers)
f_get_pointers = open_with_header(args.get_pointers)

for category in ('IMPORT','EXPORT','INTERNAL'):
    for item in specs[category].to_dict('records'):
        spec = MAPL_DataSpec.MAPL_DataSpec(category.lower(), item)
        f_specs[category].write(spec.emit_specs())
        f_declare_pointers.write(spec.emit_declare_pointers())
        f_get_pointers.write(spec.emit_get_pointers())

for category, f in f_specs.items():
    f.close()
f_declare_pointers.close()
f_get_pointers.close()



            
            

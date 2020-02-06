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
parser.add_argument('--declare_specs', action='store', default='declare_specs.h')
parser.add_argument('--declare_local', action='store', default='declare_local.h')
parser.add_argument('--get_pointer', action='store', default='get_pointer.h')
args = parser.parse_args()


f_spec = open(args.declare_specs,'w') 
f_local = open(args.declare_local,'w') 
f_get_pointer = open(args.get_pointer,'w') 

specs = reader.read_specs(args.input)
for category in ('IMPORT','EXPORT','INTERNAL'):
    for item in specs[category].to_dict('records'):
        spec = MAPL_DataSpec.MAPL_DataSpec(category.capitalize(), item)
        f_spec.write(spec.emit_spec())
        f_local.write(spec.emit_declare_local())
        f_get_pointer.write(spec.emit_get_pointer())

f_spec.close()
f_local.close()
f_get_pointer.close()

#!/usr/bin/env python3
from yaml import load,dump
import argparse
import os
import yaml
import glob

dims_dict = {"2":"xy", "3":"xyz"}

def get_dims(component_map, name):
    for comp in component_map:
        if name in component_map[comp]:
             dims = component_map[comp][name]["dims"]
    return dims

def get_vars_needed(input_file):

    output_list = []
    f = open(input_file,"r")
    lines = f.readlines()
    f.close()
    for line in lines:
        temp = line.split() 
        output_list.append(temp[4])

    return output_list

def get_extdata_map(input_dir):

    input_files = glob.glob(input_dir+"/*.yaml")
    export_list = {}
    for input_file in input_files:
       f = open(input_file,'r')
       extdata_def = yaml.safe_load(f)
       f.close()
       for short_name in extdata_def["Exports"]:
           export_list.update({short_name:extdata_def["Exports"][short_name]})
    return export_list    

def get_block(cvs_file):

    temp = cvs_file[0].split(' ')
    state_type = temp[1][1:].strip()
    component = temp[4].strip()
    i=2
    for line in cvs_file[2:]:
        if "spec for" in line:
            break
        i=i+1
    return component,state_type,i
        

def get_component_map(cvs_file):

    i_start = 0
    i_end = 0
    n_lines = len(cvs_file)
    components = {}
    while i_start < n_lines-1:

       comp_name,state_type,i_end = get_block(cvs_file[i_start:])
       comp_map = {}
       for i in range(i_end-2):
           line = cvs_file[i_start+2+i]
           values = line.split(',')
           short_name = values[1].strip()
           long_name = values[2].strip()
           units = values[3].strip()
           dims = values[4].strip()
           item_type = values[5].strip()
           comp_map.update({short_name:{"long_name":long_name,"units":units,"item_type":item_type,"dims":dims}})
       components.update({comp_name+"_"+state_type:comp_map})
       i_start = i_start + i_end

    return components

def parse_args():
    p = argparse.ArgumentParser(description='Generarte input files for ExtDataDriver to simulate GEOS')
    p.add_argument('extdata_provided',type=str,help='a list of items ExtData should fill',default=None)
    p.add_argument('spec_def',type=str,help='the GEOS gcm import state from the printspec',default=None)
    p.add_argument('extdata_dir',type=str,help='diretory with all the yaml imputs for extdata',default=None)
    p.add_argument('-e','--export',action='store_true',help='also include exports for corresponding imports')

    return vars(p.parse_args())

if __name__ == '__main__':

   args = parse_args()

   extdata_list = args['extdata_provided']
   do_exports = args['export']
   input_file = args['spec_def']
   f = open(input_file,"r")
   input_rc = f.readlines()
   f.close()

   extdata_directory = args['extdata_dir']
   extdata_def = get_extdata_map(extdata_directory)
   
   f_agcm = open("AGCM.rc",'w')
#  component
   component_map = {}
   component_map = get_component_map(input_rc)

   vars_needed = get_vars_needed(extdata_list)

   nl = "\n"
   cm = " , "

#  Import state
   written = []
   f_agcm.write("IMPORT_STATE::"+nl)

   for item in vars_needed:
      if item in extdata_def:
         long_name = "NA"
         units = "NA"
         dims = get_dims(component_map, item) 
         cdims = dims_dict[dims]

         if item not in written:
            f_agcm.write(item+cm+long_name+cm+units+cm+cdims+cm+"c"+nl)
            written.append(item)

   f_agcm.write("::"+nl)

#  Export state
   if do_exports:
       written = []
       f_agcm.write("EXPORT_STATE::"+nl)
       for item in vars_needed:
          if item in extdata_def:
             long_name = "NA"
             units = "NA"
             dims = get_dims(component_map, item) 
             cdims = dims_dict[dims]

             if item not in written:
                f_agcm.write(item+cm+long_name+cm+units+cm+cdims+cm+"c"+nl)
                written.append(item)

       f_agcm.write("::"+nl)

       f_hist = open("HISTORY.rc",'w')
       f_hist.write("GRID_LABELS:"+nl)
       f_hist.write("::"+nl)
       f_hist.write("COLLECTIONS: my_collection"+nl)
       f_hist.write("::"+nl)
       f_hist.write("my_collection.template: 'nc4'"+nl)
       f_hist.write("my_collection.format: 'CFIO'"+nl)
       f_hist.write("my_collection.frequency: '240000'"+nl)
       first = True
       written = []
       for item in vars_needed:
          if item in extdata_def:
             if item not in written:
                 if first:
                    first = False
                    f_hist.write("my_collection.fields:'"+item+"' , 'Root',"+"\n")
                 else:
                    f_hist.write("'"+item+"' , 'Root',"+"\n")
                 written.append(item)
       f_hist.write("::")

   f_hist.close()
   f_agcm.close()


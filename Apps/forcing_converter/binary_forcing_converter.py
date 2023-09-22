#!/usr/bin/env python3

import argparse
from tile_formatter import create_tile_from_map
from ll_formatter import create_latlon_from_map
from cs_formatter import create_cube_from_map
import yaml
import textwrap

def parse_args():
   program_description = textwrap.dedent(f'''
      USAGE:
      This script will take a series of MAPL binary forcing files and convert to NetCDF.

      The current binary forcing files consist of 2 records for each time. So for N times the Fortran file has 2N records.
      The first record is a real array of size 14 that encodes the valid range of the data for that record
      THe second record is the binary data array

      Currently this script takes a single yaml file as the first argument and can take optional arguement if you want verbose output to see the name of each file is processes.

      The YAML files consists of a dictionary whose values are the file path you want to convert.
      The values of the dictionary are the various keys that describe what is in the file and the ouput.
      For each entry in the map, you must at least supply the following keys:
      output_file: name of output file
      var: name of the variable in the output file
      grid_type: what type of grid the input is on, options, 'latlon','cube','tile'                                   
      In all of them the units and long_name are optional, if not passed will be set to NA
      There are two other optional keys, if neither is passed every record will be written
      The reason for using these is that ExtData does not need this "padding" and is in fact detrimental
      clim: default False, if True assumes we have a 14 month climatology and does not write first or last value
      year: default None, if passed must be integer year and only records for that year are written
      Finally as you can see in the examples, if lat-lon or cube you must tell what is the resolution via extra keys since the input files are not self descripting                                
      Also note that for the cube, you must provide a file with the coordinates since those can't be easily computed unlike the lat-lon grid

      Here is an example input demonstrating all 3 types than can be handled

      CF0048x6C/lai_clim_48x288.data:
         output_file: lai_clim_48x244.nc4
         clim: True
         var: lai
         units: 1
         long_name: leaf_area_index
         grid_type: tile
      dataoceanfile_OSTIA_REYNOLDS_SST.2880x1440.2015.data:
         output_file: dataoceanfile_OSTIA_REYNOLDS_SST.2880x1440.2015.nc4
         im: 2880
         jm: 1440
         var: sst
         units: K
         long_name: sea_surface_temperature
         year: 2015
         grid_type: latlon
      dataoceanfile_OSTIA_REYNOLDS_SST.90x540.2015.data:
         output_file: dataoceanfile_OSTIA_REYNOLDS_SST.90x540.2015.nc4
         im: 90
         var: sst
         units: K
         long_name: sea_surface_temperature
         year: 2015
         grid_type: cube
         example_file: /discover/nobackup/bmauer/Example_GEOS_Output/c90_example.20000414_2200z.nc4
      ''')
   p = argparse.ArgumentParser(description='forcing_converter',epilog=program_description,formatter_class=argparse.RawDescriptionHelpFormatter)
   p.add_argument('input_yaml',type=str,help='input file yaml file',default=None)
   p.add_argument('-v','--verbose',action='store_true',help='verbose mode')
   return vars(p.parse_args())

if __name__ == '__main__':
   args = parse_args()
   input_yaml = args['input_yaml']
   verbose = args['verbose']
   f = open(input_yaml,'r')
   files = yaml.safe_load(f)
   f.close()

   for file in files:
       if verbose:
          print(file)

       input_file = file
       grid_type = files[file]['grid_type']
       try:
          if grid_type == "tile":
             create_tile_from_map(input_file,files[file])
          elif grid_type == "latlon":
             create_latlon_from_map(input_file,files[file])
          elif grid_type == "cube":
             create_cube_from_map(input_file,files[file])
          else:
             raise ValueError()
       except ValueError as err:
          print("Incorrect grid type specified")

from netCDF4 import Dataset
import numpy as np
from scipy.io import FortranFile
from datetime import datetime
import os
import utilities as util
import yaml

def create_tile_from_map(input_file,spec):
    output_file = spec['output_file']
    var_name = spec['var']
    units = "NA"
    if 'units' in spec.keys():
       units = spec['units']
    long_name = "NA"
    if 'long_name' in spec.keys():
       units = spec['long_name']
    is_clim = False
    if 'clim' in spec.keys():
       is_clim = spec['clim']
    create_tiled_file(input_file,output_file,var_name,units,long_name,is_clim)
   
def create_tiled_file(input_file,output_file,var_name,units,long_name,is_clim):

   ndates,num_tiles,year_start,year_end = util.get_record_info(input_file,None)

   ncfid = Dataset(output_file,mode='w',format='NETCDF4')
   dimensions = {"tile_index":num_tiles}
   for key in dimensions:
       newDim = ncfid.createDimension(key,dimensions[key])

   time_dim_size = ndates
   if is_clim:
      time_dim_size = ndates - 2

   newDim = ncfid.createDimension('time',time_dim_size)
   timeVar = ncfid.createVariable("time",'f8',"time")

   file_var = ncfid.createVariable(var_name,'f4',["time","tile_index"])
   setattr(file_var,'units',units)
   setattr(file_var,'long_name',long_name)

   binary_file = FortranFile(input_file,'r')
   mid_times = []
   # since these inputs are all climatologies with the first time as a pad
   # that is not needed by ExtData, read it, but do not write it to the file
   stop_index = ndates
   if is_clim:
     hdr = binary_file.read_reals(dtype=np.float32)
     bin_var = binary_file.read_reals(dtype=np.float32)
     stop_index = ndates - 2
   for i in range(stop_index):
      hdr = binary_file.read_reals(dtype=np.float32)
      bin_var = binary_file.read_reals(dtype=np.float32)
      hdr[0]=hdr[0]+2000
      hdr[6]=hdr[6]+2000
      int_time = hdr.astype(int)
      mid_time = util.compute_time_midpoint(int_time)
      mid_times.append(mid_time)
      file_var[i,:]=bin_var


   setattr(timeVar,'units','days since '+str(mid_times[0]))
   timeVar[:] = util.compute_time_variable(mid_times)

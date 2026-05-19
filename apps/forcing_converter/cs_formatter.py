from netCDF4 import Dataset
import numpy as np
from scipy.io import FortranFile
from datetime import datetime
import os
import utilities as util
import yaml

def create_cube_from_map(input_file,spec):
    output_file = spec['output_file']
    example_file = spec['example_file']
    im = spec['im']
    var_name = spec['var']
    units = "NA"
    if 'units' in spec.keys():
       units = spec['units']
    long_name = "NA"
    if 'long_name' in spec.keys():
       long_name = spec['long_name']
    is_clim = False
    if 'clim' in spec.keys():
       is_clim = spec['clim']
    year = None
    if 'year' in spec.keys():
       year = spec['year']
    create_cube_file(input_file,output_file,example_file,var_name,units,long_name,im,is_clim,year)

def create_dimensions(ncfid,im):
    dimensions = {"Xdim":im,"Ydim":im,"nf":6}
    for key in dimensions:
        newDim = ncfid.createDimension(key,dimensions[key])

def create_dim_vars(ncfid,im,example_file):
    vXdim = ncfid.createVariable('Xdim','f8',('Xdim'))
    vYdim = ncfid.createVariable('Ydim','f8',('Ydim'))
    setattr(ncfid.variables['Xdim'],'units','degrees_east')
    setattr(ncfid.variables['Ydim'],'units','degrees_north')
    setattr(ncfid.variables['Xdim'],'long_name','Fake Longitude for GrADS Compatibility')
    setattr(ncfid.variables['Ydim'],'long_name','Fake Latitude for GrADS Compatibility')
    vXdim[:]=range(1,im+1)
    vYdim[:]=range(1,im+1)
    vnf = ncfid.createVariable('nf','i4',('nf'))
    vnf[:]=range(1,7)
    setattr(ncfid.variables['nf'],'long_name','cubed-sphere face')
    setattr(ncfid.variables['nf'],'axis','e')
    setattr(ncfid.variables['nf'],'grads_dim','e')

    vlon = ncfid.createVariable('lons','f8',('nf','Ydim','Xdim'))
    setattr(ncfid.variables['lons'],'units','degrees_east')
    setattr(ncfid.variables['lons'],'long_name','longitude')

    vlat = ncfid.createVariable('lats','f8',('nf','Ydim','Xdim'))
    setattr(ncfid.variables['lats'],'units','degrees_north')
    setattr(ncfid.variables['lats'],'long_name','latitude')
    
    ncFidEx = Dataset(example_file,mode='r')
    vlon[:,:,:]=ncFidEx.variables['lons'][:,:,:]
    vlat[:,:,:] = ncFidEx.variables['lats'][:,:,:]

def create_time_and_var_cs(input_file,ncfid,var_name,units,long_name,is_clim,year,im):

   ndates,record_size,year_start,year_end = util.get_record_info(input_file,year)
   assert( im*im*6 == record_size),"input im  does not match file"
   time_dim_size = ndates
   start_index = 0
   stop_index = ndates
   offset_year = 0
   if is_clim:
     start_index = 1
     stop_index = ndates-1
     time_dim_size = ndates - 2
     offset_year = 2000
   if year != None:
     start_index = year_start
     stop_index = year_end
     time_dim_size = year_end-year_start

   f = FortranFile(input_file,'r')
   newDim = ncfid.createDimension('time',time_dim_size)
   timeVar = ncfid.createVariable("time",'f8',"time")

   sstVar = ncfid.createVariable(var_name,'f4',["time","nf","Ydim","Xdim"])
   setattr(sstVar,'units',units)
   setattr(sstVar,'long_name',long_name)
   setattr(sstVar,'grid_mapping','cubed_sphere')
   setattr(sstVar,'coordinates','lons lats')

   mid_times = []
   put_index = 0
   for i in range(ndates):
      hdr = f.read_reals(dtype=np.float32)
      bin_var = f.read_reals(dtype=np.float32)
      hdr[0]=hdr[0]+offset_year
      hdr[6]=hdr[6]+offset_year
      if (i >= start_index) and (i < stop_index): 
          int_time = hdr.astype(int)
          mid_time = util.compute_time_midpoint(int_time)
          mid_times.append(mid_time)
          sstVar[put_index,:,:,:]=bin_var
          put_index = put_index + 1


   setattr(timeVar,'units','days since '+str(mid_times[0]))

   times = np.zeros(ndates,dtype=np.float64)
   timeVar[:] = util.compute_time_variable(mid_times)

def create_cube_file(input_file,output_file,example_file,var_name,units,long_name,im,is_clim,year):

   if is_clim and (year != None):
      raise Exception("cannot specify a year and climatology")

   ncfid = Dataset(output_file,mode='w',format='NETCDF4')
   create_dimensions(ncfid,im)
   create_dim_vars(ncfid,im,example_file)
   create_time_and_var_cs(input_file,ncfid,var_name,units,long_name,is_clim,year,im)
   

from netCDF4 import Dataset
import numpy as np
from scipy.io import FortranFile
from datetime import datetime
import os
import utilities as util
import yaml

def create_latlon_from_map(input_file,spec):
    output_file = spec['output_file']
    im = spec['im']
    jm = spec['jm']
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
    create_latlon_file(input_file,output_file,var_name,units,long_name,im,jm,is_clim,year)
    
def compute_lons(im):
    dr=360.0/im
    lons = np.arange(-180.0+dr/2.0,180.0+dr/2.0,dr,dtype=np.float64)
    return(lons)

def compute_lats(im):
    dr=180.0/im
    lats = np.arange(-90.0+dr/2.0,90.0+dr/2.0,dr,dtype=np.float64)
    return(lats)

def create_dimensions_ll( ncfid,im,jm):
    dimensions = {"lon":im,"lat":jm}
    for key in dimensions:
        newDim = ncfid.createDimension(key,dimensions[key])

def create_dim_vars_ll(ncfid,im,jm):

   lonVar = ncfid.createVariable("lon",'f8',"lon")
   setattr(lonVar,'units','degrees_east')
   setattr(lonVar,'long_name','Longitude')

   latVar = ncfid.createVariable("lat",'f8',"lat")
   setattr(latVar,'units','degrees_north')
   setattr(latVar,'long_name','Latitude')

   lonVar[:]=compute_lons(im)
   latVar[:]=compute_lats(jm)

def create_time_and_var_ll(input_file,ncfid,var_name,units,long_name,is_clim,year,im,jm):

   ndates,record_size,year_start,year_end = util.get_record_info(input_file,year)
   assert( im*jm == record_size),"input im and jm does not match file"
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

   sstVar = ncfid.createVariable(var_name,'f4',["time","lat","lon"])
   setattr(sstVar,'units',units)
   setattr(sstVar,'long_name',long_name)

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
          sstVar[put_index,:,:]=bin_var
          put_index = put_index + 1


   setattr(timeVar,'units','days since '+str(mid_times[0]))

   times = np.zeros(ndates,dtype=np.float64)
   timeVar[:] = util.compute_time_variable(mid_times)

def create_latlon_file(input_file,output_file,var_name,units,long_name,im,jm,is_clim,year):

   if is_clim and (year != None):
      raise Exception("cannot specify a year and climatology")

   ncfid = Dataset(output_file,mode='w',format='NETCDF4')
   create_dimensions_ll(ncfid,im,jm)
   create_dim_vars_ll(ncfid,im,jm)
   create_time_and_var_ll(input_file,ncfid,var_name,units,long_name,is_clim,year,im,jm)
   

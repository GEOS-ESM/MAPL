#!/usr/bin/env python

#-------------
# Load modules
#-------------
from netCDF4 import Dataset
import numpy
import argparse
import yaml

def parse_args():
    p = argparse.ArgumentParser(description='Flatten a lat-lon to 1D')
    p.add_argument('input',type=str,help='input file',default=None)
    p.add_argument('nfiles',type=int,help='number of files',default=None)
    p.add_argument('output',type=str,help='output file',default=None)
    return vars(p.parse_args())

#------------------
# Opening the file
#------------------
comm_args    = parse_args()
Input_template   = comm_args['input']
num_files = comm_args['nfiles']
Output_file  = comm_args['output']


ncFid = Dataset(Input_template+"_"+str(1), mode='r')
ncFidOut = Dataset(Output_file, mode='w', format='NETCDF4')

res = len(ncFid.dimensions['lon'])
j_per_file = res*6//num_files
#---------------------
# Extracting variables
#---------------------

old_time = ncFid.variables['time'][:]

exclude_dims = ['time','lon','lat']
detected_dims = []
for dim in ncFid.dimensions:
    if dim not in exclude_dims:
           detected_dims.append(dim)
cube_res = len(ncFid.dimensions['lon'])

# define dimenions

Xdim = ncFidOut.createDimension('lon',cube_res)
Ydim = ncFidOut.createDimension('lat',cube_res*6)

for dim in detected_dims:
    dim_out = ncFidOut.createDimension(dim,len(ncFid.dimensions[dim]))

new_time_dim = ncFidOut.createDimension('time',1)

# define coordinate variables

new_time = ncFidOut.createVariable('time','f8',('time'))
for att in ncFid.variables['time'].ncattrs():
   for att in ncFid.variables['time'].ncattrs():
      setattr(ncFidOut.variables['time'],att,getattr(ncFid.variables['time'],att))
   new_time[:] = 0


vXdim = ncFidOut.createVariable('lon','f8',('lon'))
vYdim = ncFidOut.createVariable('lat','f8',('lat'))
setattr(ncFidOut.variables['lon'],'units','degrees_east')
setattr(ncFidOut.variables['lat'],'units','degrees_north')
setattr(ncFidOut.variables['lon'],'long_name','longitude')
setattr(ncFidOut.variables['lat'],'long_name','latitude')
vXdim[:]=range(1,cube_res+1)
vYdim[:]=range(1,(cube_res*6)+1)

for dim in detected_dims:
    if dim in ncFid.variables:
        vLevOut = ncFidOut.createVariable(dim,'f8',(dim))
        for att in ncFid.variables[dim].ncattrs():
            setattr(ncFidOut.variables[dim],att,getattr(ncFid.variables[dim],att))
        dim_size = len(ncFid.dimensions[dim])+1
        vLevOut[:] = range(1,dim_size)

# special handling if fvcore restart for AK/BK or pref
oned_vars = ['AK','BK','PREF']
for oned_var in oned_vars:
    if oned_var in ncFid.variables:
       float_type = ncFid.variables[oned_var].dtype
       ak= ncFidOut.createVariable(oned_var,float_type,('edge'))
       for att in ncFid.variables[oned_var].ncattrs():
          setattr(ncFidOut.variables[oned_var],att,getattr(ncFid.variables[oned_var],att))
       ak[:] = ncFid.variables[oned_var][:]

ncFid.close()

Exclude_Var = ['time','edge','lev','lon','lat','AK','BK','unknown_dim1','unknown_dim2']

for i in range(num_files):
    ncFid = Dataset(Input_template+"_"+str(i), mode='r')
    if i==0:
        for var in ncFid.variables:
           if var not in Exclude_Var:
              temp = ncFid.variables[var][:]
              dim_size =len(temp.shape)
              float_type = ncFid.variables[var].dtype
              var_dims = ncFid.variables[var].dimensions

              if dim_size == 4:
                 tout = ncFidOut.createVariable(var,float_type,var_dims,fill_value=1.0e15,chunksizes=(1,1,cube_res,cube_res))
                 for att in ncFid.variables[var].ncattrs():
                    if att != "_FillValue":
                       setattr(ncFidOut.variables[var],att,getattr(ncFid.variables[var],att))
              elif dim_size == 3:
                 tout = ncFidOut.createVariable(var,float_type,var_dims,fill_value=1.0e15,chunksizes=(1,cube_res,cube_res))
                 for att in ncFid.variables[var].ncattrs():
                    if att != "_FillValue":
                       setattr(ncFidOut.variables[var],att,getattr(ncFid.variables[var],att))
              elif dim_size == 2:
                 tout = ncFidOut.createVariable(var,float_type,('lat','lon'),fill_value=1.0e15,chunksizes=(cube_res,cube_res))
                 for att in ncFid.variables[var].ncattrs():
                    if att != "_FillValue":
                       setattr(ncFidOut.variables[var],att,getattr(ncFid.variables[var],att))

    for var in ncFid.variables:
       if var not in Exclude_Var:
          temp = ncFid.variables[var][:]
          dim_size =len(temp.shape)
          tout = ncFidOut.variables[var][:]

          if dim_size == 4:
             il =  j_per_file*i
             iu =  j_per_file*(i+1)
             ncFidOut.variables[var][:,:,il:iu,:] = temp[:,:,:,:]

          elif dim_size == 3:
             il =  j_per_file*i
             iu =  j_per_file*(i+1)
             ncFidOut.variables[var][:,il:iu,:] = temp[:,:,:]

          elif dim_size == 2:
             il =  j_per_file*i
             iu =  j_per_file*(i+1)
             ncFidOut.variables[var][il:iu,:] = temp[:,:]

    ncFid.close()

#-----------------
# Closing the file
#-----------------
ncFidOut.close()


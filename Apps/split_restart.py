#!/usr/bin/env python

#-------------
# Load modules
#-------------
from netCDF4 import Dataset
import numpy
import argparse
import sys

def parse_args():
    p = argparse.ArgumentParser(description='Flatten a lat-lon to 1D')
    p.add_argument('input',type=str,help='input file',default=None)
    p.add_argument('output',type=str,help='output file',default=None)
    p.add_argument('split',type=int,help='number of files to split into',default=None)
    return vars(p.parse_args())

#------------------
# Opening the file
#------------------
comm_args    = parse_args()
Input_file   = comm_args['input']
Output_template  = comm_args['output']
n_files = comm_args['split']

ncFid = Dataset(Input_file,mode='r')

if 'tile' in ncFid.dimensions:
   quit()

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

Exclude_Var = ['time','edge','lev','lon','lat','AK','BK','unknown_dim1','unknown_dim2']

remainder = (cube_res*6)%n_files
if remainder != 0:
   raise ValueError('number of files my evenly divide 6 times cube size')

y_size = cube_res*6//n_files

# create master file
f = open(Output_template,mode='w')
out_master = "num_files: "+str(n_files)+"\n"+"j_size: "+str(cube_res)
f.write(out_master)
f.close()
# create each file
for i in range(n_files):
    ncFidOut = Dataset(Output_template+"_"+str(i), mode='w',format='NETCDF4')
    setattr(ncFidOut,'Split_Cubed_Sphere',i)

    # define dimenions

    Xdim = ncFidOut.createDimension('lon',cube_res)
    Ydim = ncFidOut.createDimension('lat',y_size)

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
    y_start = i*y_size
    vXdim[:]=range(1,cube_res+1)
    vYdim[:]=range(1+y_start,y_size+1+y_start)

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

    # define variables
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
             il =  y_size*i
             iu =  y_size*(i+1)
             ncFidOut.variables[var][:,:,:,:] = temp[:,:,il:iu,:]

          elif dim_size == 3:
             il =  y_size*i
             iu =  y_size*(i+1)
             ncFidOut.variables[var][:,:,:] = temp[:,il:iu,:]

          elif dim_size == 2: 
             il =  y_size*i
             iu =  y_size*(i+1)
             ncFidOut.variables[var][:,:] = temp[il:iu,:]

    ncFidOut.close()

#-----------------
# Closing the file
#-----------------
ncFid.close()


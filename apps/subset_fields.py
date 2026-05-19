#!/usr/bin/env python

#-------------
# Load modules
#-------------
from netCDF4 import Dataset
import argparse

def parse_args():
    p = argparse.ArgumentParser(description='subset grids')
    p.add_argument('-i','--input',type=str,help='input file',default=None)
    p.add_argument('-o','--output',type=str,help='output file',default='out.nc4')
    p.add_argument('-x1', '--xcoord1', type=int, help='x coord of bottom left point', default=0)
    p.add_argument('-y1', '--ycoord1', type=int, help='y coord of bottom left point', default=24)
    p.add_argument('-x2', '--xcoord2', type=int, help='x coord of top right point', default=12)
    p.add_argument('-y2', '--ycoord2', type=int, help='y coord of top right point', default=48)
    return vars(p.parse_args())

#------------------
# Opening the file
#------------------
comm_args    = parse_args()
Input_file   = comm_args['input']
Output_file  = comm_args['output']
x1 = comm_args['xcoord1']
y1 = comm_args['ycoord1']
x2 = comm_args['xcoord2']
y2 = comm_args['ycoord2']
ncFid = Dataset(Input_file, mode='r')
ncFidOut = Dataset(Output_file, mode='w', format='NETCDF4')

#---------------------
# Extracting variables
#---------------------
   
lat  = ncFid.variables['lat'][:]
lon  = ncFid.variables['lon'][:]

for att in ncFid.ncattrs():
    setattr(ncFidOut,att,getattr(ncFid,att))

latOut = ncFidOut.createDimension('lat', y2-y1) 
latsOut = ncFidOut.createVariable('lat','f8',('lat',))
for att in ncFid.variables['lat'].ncattrs():
    setattr(ncFidOut.variables['lat'],att,getattr(ncFid.variables['lat'],att))
latsOut[:] = lat[y1:y2]

lonOut = ncFidOut.createDimension('lon', x2-x1)
lonsOut = ncFidOut.createVariable('lon','f8',('lon',))
for att in ncFid.variables['lon'].ncattrs():
    setattr(ncFidOut.variables['lon'],att,getattr(ncFid.variables['lon'],att))
lonsOut[:] = lon[x1:x2]

exclude_dim = ['lon', 'lat']
for dim in ncFid.dimensions:
    if dim not in exclude_dim:
        if "unknown_dim" not in dim:
            temp = ncFid.variables[dim][:]
            dimOut = ncFidOut.createDimension(dim, len(temp))
            dimVarOut = ncFidOut.createVariable(dim, 'f8', dim)
            for att in ncFid.variables[dim].ncattrs():
                setattr(ncFidOut.variables[dim],att,getattr(ncFid.variables[dim],att))
            dimVarOut[:] = temp
        # for special dimensions in GOCART components
        else:
            dim_size = ncFid.dimensions[dim].size
            dimOut = ncFidOut.createDimension(dim, dim_size)
        
exclude_var = ncFid.dimensions
for var in ncFid.variables:
    if var not in exclude_var:
        temp = ncFid.variables[var][:]
        dim_size =len(temp.shape)
        dims = ncFid.variables[var].dimensions
        Tout = ncFidOut.createVariable(var, 'f4', dims, fill_value=1.0e15)

        for att in ncFid.variables[var].ncattrs():
            if att != "_FillValue":
                setattr(ncFidOut.variables[var], att, getattr(ncFid.variables[var],att))
                if dim_size == 4:
                    Tout[:,:,:,:] = temp[:,:,y1:y2,x1:x2]
                elif dim_size == 3:
                    Tout[:,:,:] = temp[:,y1:y2,x1:x2]
                elif dim_size == 2:
                    Tout[:,:] = temp[y1:y2,x1:x2]
                elif dim_size == 1:
                    Tout[:] = temp[:]

ncFidOut.close()
ncFid.close()



#define I_AM_MAIN
#include "MAPL_Generic.h"

program  ABI_fixed_coord

   use ESMF
   use MAPL
   use MAPL_FileMetadataUtilsMod
   use gFTL_StringVector
   use MPI
   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   use ieee_arithmetic, only: isnan => ieee_is_nan
   use Plain_netCDF_Time
   implicit none


   character*150 ::  fn, kx, ky
   character*150 ::  var_name_proj, att_name_proj
   integer :: nx, ny
   real(REAL64), allocatable :: x(:), y(:) 
   real(REAL64) :: lambda0, lambda0_deg

   integer :: status
   
   fn='/Users/yyu11/ModelData/Data_geosrun_2023/GOES-16-ABI/OR_ABI-L1b-RadF-M6C04_G16_s20192340800216_e20192340809524_c20192340809552.nc'

   kx='x'
   ky='y'
   call get_ncfile_dimension(fn, nlon=nx, nlat=ny, key_lon=kx, key_lat=ky, _RC)
   write(6,121) 'nx, ny', nx, ny

   allocate(x(nx))
   allocate(y(ny))   
   call get_v1d_netcdf_R8_complete (fn, kx, x, _RC)
   call get_v1d_netcdf_R8_complete (fn, ky, y, _RC)   
   write(6, 101) 'x=', x
   write(6, 101) 'y=', y

   var_name_proj='goes_imager_projection'
   att_name_proj='longitude_of_projection_origin'
   call get_att_real_netcdf( fn, var_name_proj, att_name_proj, lambda0_deg, _RC)
   lambda0 = lambda0_deg/180.d0*4.d0*atan(1.d0)

   write(6, 101) 'lambda0=', lambda0   
   
   include '/Users/yyu11/sftp/myformat.inc'
 end program ABI_fixed_coord

# Introduction
MAPL has a regridding class with underlying ESMF regridding capabilities as a base but provides extensions on top of this that can be invoked in a Fortran procedure that uses MAPL. Note all regridding methods respect MAPL_UNDEF and will not include the contributions any source point that is MAPL_UNDEF when computing destination point. History and ExtData both uses these methods internally.

# Using MAPL Regridders in Code
MAPL provides a neat class that wraps the ESMF regridders as well as extended functionality via custom dynamic masks as well as vector regridding (note that we assume all vector quantities are co-located A-grid quantities). In the vector case the longitudinal and latitudinal tangential components are converted to an xyz cartesian, each component is regridded, then this is transformed back to the tangential components on the sphere. In addition we have a regridder manager type that instantiates a singleton instance of the class to store and re-use existing regridder rather than regenerate the regridding weights each time they are needed. To use the regridders it as simple as the following example code.

```
class(AbstractRegridder), pointer :: regridding_handle

! get the input and output grids from the fields you want to regrid
! via the standard ESMF_FieldGet calls

regridding_handle => new_regridder_manager%make_regridder(input_grid,output_grid,regrid_method,rc=status)

```
In the code above we passed two grids, and a regridding method (which is an integer) and the new_regridder_manager is the singleton instance mentioned. 

**Note the one requirement is that to use this both grids must have been made with the MAPL grid manager.** Once you have the pointer to the regridding handle it can be used to regrid any 2 fields on that input/output grid combination. Note that the fields can have any ungridded dimensions as long as the input and output have the same ungridded dimensions.

The following integer constants corresponding to the available  options are available for the regrid methods and available:    
- REGRID_METHOD_IDENTITY
- REGRID_METHOD_BILINEAR
- REGRID_METHOD_BILINEAR_ROTATE
- REGRID_METHOD_CONSERVE
- REGRID_METHOD_VOTE
- REGRID_METHOD_FRACTION
- REGRID_METHOD_CONSERVE_2ND
- REGRID_METHOD_PATCH
- REGRID_METHOD_NEAREST_STOD
- REGRID_METHOD_CONSERVE_HFLUX
- REGRID_METHOD_BILINEAR_MONOTONIC
- REGRID_METHOD_CONSERVE_MONOTONIC

Once you have the regridding handle it can be used like so:

```
class(AbstractRegridder), pointer :: regridding_handle
type(ESMF_Field) :: field_2d_in,field_2d_out
type(ESMF_Field) :: field_3d_in,field_3d_out
type(ESMF_Field) :: field_u_in,field_v_in,field_v_out,field_v_out

type(ESMF_Grid) :: input_grid, output_grid

real, pointer :: ptr2d_in(:,:), ptr2d_out(:,:)
real, pointer :: ptr3d_in(:,:,:), ptr3d_out(:,:,:)
real, poitner :: ptr_u_in(:,:), ptr_v_in(:,:), ptr_u_out(:,:), ptr_v_out(:,:)

call ESMF_FieldGet(field_2d_in,grid=input_grid)
call ESMF_FieldGet(field_2d_out,grid=output_grid)

regridding_handle => new_regridder_manager%make_regridder(input_grid,output_grid,regrid_method,rc=status)

! now if field_3d_in and field_u_in and field_v_in are all on the same grid
! and if field_3d_out and field_u_out and field_v_out are all on the same grid
! the just generated handle can be used for all of these like so

call ESMF_FieldGet(field_2d_in,0,farrayPtr=ptr2d_in)
call ESMF_FieldGet(field_2d_out,0,farrayPtr=ptr2d_out)
call ESMF_FieldGet(field_3d_in,0,farrayPtr=ptr3d_in)
call ESMF_FieldGet(field_3d_out,0,farrayPtr=ptr3d_out)
call ESMF_FieldGet(field_u_in,0,farrayPtr=ptr_u_in)
call ESMF_FieldGet(field_v_in,0,farrayPtr=ptr_v_in)
call ESMF_FieldGet(field_u_out,0,farrayPtr=ptr_u_out)
call ESMF_FieldGet(field_v_out,0,farrayPtr=ptr_v_out)

call regrid_handle%regrid(ptr2d_in,ptr2d_out,rc=status)
call regrid_handle%regrid(ptr3d_in,ptr3d_out,rc=status)
! And if you want to regrid 2 fields as a vector you can do it like this
! still with the same regridding handle
call regrid_handle%regrid(ptr_u_in,ptr_v_in,ptr_u_out,ptr_v_out)
! although not demonstrated, if the fields contained double-precision data
! you can do that too, it has overloads for double precision pointers

```

# Specifying Regridding Methods in ExtData and History (in MAPL v2.22.0 and greater)
keyword values for regrid_method key in both a History collection, ExtData2G entry, and Regrid_Util.x:
* `BILINEAR` - ESMF bilinear
* `CONSERVE` - ESMF 1st order conservative
* `VOTE` -  ESMF custom dynamic regridding options on 1st order conservative regridding to implement method that  provide for “voting” (the value from the source cell with the greatest overlap with the destination cell is used as the value of the destination). Useful for source data that represents a discrete value, like some sort of region mask.
* `FRACTION` - ESMF custom dynamic regridding options on 1st order conservative regridding to implement method that  provide for “fraction” (what fraction of tiles on exchange grid have a specific value)
* `CONSERVE_2ND` - ESMF 2nd order conservative
* `PATCH` - ESMF 2nd order bilinear
* `CONSERVE_HFLUX` - regrid horizontal fluxes in an exact manner for integral grid resolution ratios. Really should only be used when reading in mass fluxes in ExtData. 
* `BILINEAR_MONOTONIC` - ESMF bilinear + custom dynamic mask to ensure they any destination values that are greater than a source value are renormalized to maximum source value. This normally should not be the case but in the event that all source points are identical or very close numerically there is a chance due to the normal imprecision of floating point math that the destination points could be a few machine epsilon greater than the source points. Use this if such a result would is unacceptable.
* `CONSERVE_MONOTONIC` - ESMF 1st order conserve + the above details the same as the BILINEAR_MONOTONIC
* `NEAREST_STOD` - ESMF nearest source to destination. In other words, the nearest neighbor


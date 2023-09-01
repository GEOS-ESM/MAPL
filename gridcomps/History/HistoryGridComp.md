
# Content

- [1 Overview](#1-overview)
- [2 Input file specification](#2-input-file-specification)
  * [2.1 Global Options](#21-global-options)
  * [2.2 Collection List](#22-collection-list)
  * [2.3 Grid Labels](#23-grid-labels)
  * [2.4 Collections](#24-collections)
- [3 Collection Keyword Descriptions](#3-collection-keyword-descriptions)
- [4 Advanced options](#4-advanced-options)
  * [4.1 Horizontal Regridding](#41-horizontal-regridding)
  * [4.2 Vertical regridding](#42-vertical-regridding)
  * [4.3 Expression in `History`](#43-expression-in--history-)
  * [4.4 Output variables in Bundles](#44-output-variables-in-bundles)
  * [4.5 Splitting 4-D fields to 3-D and 3-D Fields to 2-D](#45-splitting-4-d-fields-to-3-d-and-3-d-fields-to-2-d)
  * [4.6 Outputting monthly data](#46-outputting-monthly-data)
- [5 Tips for `History`](#5-tips-for--history-)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>


# 1 Overview
The `History` component is one of several specialized components provided by the MAPL library. `History` exists to write diagnostic data from the Export State of a ESMF gridded component in a MAPL hierarchy. 
`History` relies on a resource file (`HISTORY.rc`) that consists of "collections" which define a group of variables and the components they can be found in that are output with identical parameters. **At its most basic use, i.e. if you don't explicitly tell History to do something else, it will write the field in the native representation of as it exists in the ESMF gridded component the field comes from (i.e. on the same horizontal grid and with same number of vertical levels as in the component).**

**Remark 1:** `History` does not handle the checkpointing of the component states for subsequent use as restarts. That is a separate code from `History`.

# 2 Input file specification
The `History` resource file (`HISTORY.rc`) uses the ESMF config format. The structure is built around the concept of a collection, where a collection is a set of fields that will be written to a common file stream and processed for output with the same options. The basic `HISTORY.rc` file consists of three sections and some option keywords that apply to the output as a whole. **Note that files created will be named with the EXPID+collection_name+collection_template.** The following subsections describe the options for `HISTORY.rc`.

## 2.1 Global Options
The following are global options that may be set in the resource file:
```
EXPID: experiment id
FileOrder: optional, sets the order of the variables in the collection in the netcdf file 
           to alphabetical (default) and makes sure any variables that are part of the metadata 
           like lons or lats go first. If you don't want this for some reason set to "add_order" 
           which will just put them in the order they get added to the netcdf file.
```
## 2.2 Collection List
The collection list specifies which collections to write. Even if a collection is defined in `HISTORY.rc`, unless it is explicitly there, it will not be written. The collection list is specified as follows:
```
Collections: 'collection_a'
             'collection_b'
             'collection_c'
::
```
If you want to temporarily disable writing of a collection, just remove it from this list (or comment out by using the character `#` in front of the collection name).
You don't need to delete its definition later in `HISTORY.rc`.

## 2.3 Grid Labels
The grid label section provides a list of grid definitions that may be referred to in collections for the HORIZONTAL regridding, so the `LM` value (number of verical levels) is irrelevant. If you put something it will be ignored, the actual non-distributed dimensions of the field will be examined to make decision about how the vertical will be handled.  
These definitions specify the horizontal output grid for the collection if the user wants the output regridded to a different horizontal grid than the native grid the requested field is defined on. Currently this supports Lat-Lon and Cubed-Sphere grids. Each grid has the form of `grid_name.option` where the `grid_name` is what is referred to in the collection. Note that each grid definition must have a `GRID_TYPE` entry. The rest of the entries may be varying depending on the grid type. 

Here is an example Lat-Lon definitions. The user specifies the longitudinal (`IM_WORLD`) size, the latitudinal (`JM_WORLD`) size, the pole (options `PC` or `PE` for pole edge and pole center), and the dateline options (`DE` or `DC` for dateline edge and dateline center). 
```
PC96x49-DC.GRID_TYPE: LatLon
PC96x49-DC.IM_WORLD: 96
PC96x49-DC.JM_WORLD: 49
PC96x49-DC.POLE: PC
PC96x49-DC.DATELINE: DC
PC96x49-DC.LM: 72
```
For a complete list of supported grid types and options for each type, see the following page about creating grids from an ESMF_Config (which what the History.rc file is):

  [Creating Grids with MAPL Grid Factories](https://github.com/GEOS-ESM/MAPL/wiki/Creating-Grids-with-MAPL-Grid-Factories)

The actual grid to grid transformation is performed using ESMF and we currently support bilinear and first order conservative. For more information see:
[ESMF Regridding](https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node3.html#SECTION03023000000000000000)

## 2.4 Collections
```
coll_name.template:     grads style template that defines time characteristics of the 
                        output file, e.g. %y4%m2%d2_%h2%n2z.nc4
coll_name.format:       output file format, 'flat' binary or 'CFIO' netcdf, optional, default 'flat'
coll_name.mode:         controls time output, whether to time average or write instantaneous values. 
                        Options 'instantaneous' (default) or 'time-averaged'
coll_name.frequency:    time interval in HHMMSS format, frequency collection will be written
coll_name.duration:     time interval in HHMMSS format, define how long to write to the
                        current file before creating a new file, by default duration is the 
                        freuqency for only one time will be written to each file
coll_name.grid_label:   grid definition to use for the output horizontal regridding
coll_name.vscale:
coll_name.vunit:
coll_name.vvars:
coll_name.levels:
coll_name.ref_time:     time in HHMMSS format, optional, reference time used in 
                        conjunction with ref_date and frequency to determine when to write, 
                        optional, default 000000
coll_name.ref_date:     date in YYYYMMDD format, optional, reference date used in conjunction 
                        with ref_time and frequency to determine when to write, optional, 
                        defaults to the date of the application clock
coll_name.end_date:     date in YYYYMMDD format, optional, turns off collection at this date, 
                        by default no end date
coll_name.end_time:     time in HHMMSS format, optional, turns off collection at this time, 
                        by default no end time
coll_name.regrid_name:
coll_name.regrid_exch:
coll_name.fields:       Definition of the fields that make up the collection, described later
coll_name.monthly:
coll_name.splitField:
coll_name.UseRegex:
coll_name.nbit:         bit shaving, integer, optional, if not present, no bit shaving, 
                        otherwise integer, retain that many bits of the mantissa, 
                        useful for better compression
coll_name.deflate:      netcdf compression level, default 0, can be 0-9
coll_name.chunksize:    netcdf chunking, by default the chunksizes will match the dimension, 
                        otherwise must be a list of comma separated numbers that match 
                        the number of dimensions in the output file. For example, suppose 
                        you are outputting on a 180x90 lat-lon grid, an there are 3D variables 
                        in the file, the file will have 4 dimensions, lon,lat,lev,time 
                        so you could say 90,45,1,1
coll_name.conservative: use conservative regridding, default 0, 0 - bilinear, 1 - conservative
```

The fields entry is described in more detail here as it has several options. The entry can consist of multiple lines, each of which may have two to four entries. For example:

```
  geosgcm_prog.fields:    'PHIS'     , 'AGCM'         ,
                          'SLP'      , 'DYN'          ,
                          'U;V'      , 'DYN'          ,
                          'ZLE'      , 'DYN'          , 'H'   ,
                          'OMEGA'    , 'DYN'          ,
                          'Q'        , 'MOIST'        , 'QV'  ,      
                          ::
```

Each line consists of:
* short_name of the variable in the gridded component
* name of component the variable may be found in
* optional name to use in the output file in place of the short_name
* optional modification to the coupler if time averaging. By default the coupler time averaged over the interval, set to 'MIN' or 'MAX' if you want the minimum or maximum in the interval.
Note that in the example above the entry with U;V. This denotes that the two variables separated by the `;` represent a vector pair and if regridded to a new grid should be handled accordingly.

# 3 Collection Keyword Descriptions

* `regrid_method`: available on and after v2.22.0, regrid method to use. The options can be found the [MAPL REGRIDDING METHODS](https://github.com/GEOS-ESM/MAPL/wiki/Regridding-Methods-Available-In-MAPL) document. It is an error to specify both this and the conservative keyword.
* `conservative`: (starting from v2.22.0 new regridding keyword available, consider this depreciated when making new collections) use conservative regridding, default 0, 0 - bilinear, 1 - conservative.
* `deflate`: defaults to 0, deflation level used in NetCDF
* `frequency`: this is the frequency to output the collection in HHMMSS format.
* `levels`: list of space separated levels to output.  If no `vvars` option is specified these are the actual level indices in a Fortran sense. For example if you specify `1 2 3`, this will output the levels indexed by 1, 2, and 3 in the undistributed dimension in the underlying Fortran array. If `vvars` is specified then these are the levels that will be interpolated to and output matching the type represented by `vvars`. For example if `ZLE` is specified as `vvars`, for levels you could specify something like this `10 20 50 100 1000` which would be the heights in meters you want to output.
* `mode`: 'instantaneous' (default) or 'time-averaged', either time average the fields between writes or just output the instantaneous value.
* `nbits`: this performs "bit shaving" and sets 24-nbits of the mantissa for each value output to zero. This helps compression at the loss of some information

# 4 Advanced options
## 4.1 Horizontal Regridding
A collection can be regridded from the native horizontal grid of the fields in the collection to a different grid. This is controlled via 2 keywords, the `grid_label` and pre MAPL v2.22.0 the conservative keyword and on/after 2.22.0 the `regrid_method` keyword.

The keyword `grid_label` tells it **WHAT** grid definition to regrid the collection to.

The `regrid_label` or conservative keyword tells it **HOW** to regrid to that grid, i.e., do I want to do bilinear regridding, conservative, or some other method.

Neither of these have ANY effect on the undistributed dimensions of the field. Those could represent the model levels or something else.

## 4.2 Vertical regridding
The vertical regridding is controlled via the `vvar`, `vscale`, `vunit`, and `levels` keywords. The `grid_label`/`regrid_method` have absolutely no effect on the vertical regridding.

## 4.3 Expression in `History`

## 4.4 Output variables in Bundles

## 4.5 Splitting 4-D fields to 3-D and 3-D Fields to 2-D
A collection can have an option called `splitField`.
This is effectively a dimensionality reducer/splitter for fields. Basically any 4D dimensional fields with a trailing dimension of `N` will be split into `N` 3D fields (the names of the fields will be appended with the index number). Likewise any 3D fields with a trailing dimension of of `N` will be split into `N` 2D fields.

## 4.6 Outputting monthly data

# 5 Tips for `History`
* Unless you are interpolating to a set of levels, you can not mix variables that are defined on the center and edge in the vertical in a collection as only one vertical coordinate may be defined in the output. **If you want to output both center and edge variables on the native levels, you must write two collections**. 
* Likewise if your field has an ungridded dimension you can output it (the ungridded dimension is denoted as a level in the NetCDF file), but it can't have any vertical level as well (unless you use the splitting keyword for 4D fields ...).

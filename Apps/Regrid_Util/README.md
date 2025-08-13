# Overview
Regrid_Util.x is a utility program built with MAPL. It has the ability to regrid files created by the MAPL IO layer (newCFIO+pFIO) that is used by the MAPL History and ExtData Component to any lat-lon, cubed-sphere, or a tripolar grid. The utility can therefore regrid between any two supported grid types, and the input and output grids can be of the **same type** (but different resolution). Behind the scene it uses ESMF to perform the regridding. It is entirely driven by command line options. The user specifies an output grid and the input grid type and specifications are determined from the input file.

Note this should be able to regrid a file produced by the History component and the resultant file should be readable by ExtData. Note that the Lat-Lon files produced by History match the CF convention, so if you have your own lat-lon files that were produced by some other means, if they adhere to the CF standard for lons,lats,level, and time information, then they will probably work if you give them as input. If you want to be extra safe run GEOS and get some lat-lon History output and make sure your own files looks like this!

_**Also note, due to some misunderstandings as IO layers evolved, some metadata that is in files produced by History before MAPL 2.0 may not be preserved when running this code with version starting at 2.0+. We realized this and fixed this. You will need MAPL v2.18.0 in order to ensure that files get written with all the metadata that was in the pre MAPL 2.0 History output. Also note this missing metadata was global attributes, duplicate definitions of the missing value that are part of no standard, and extra time attributes that duplicate the information contained in the time units and time variable. In other words if your code was relying on these, you probably need to rewrite as it was badly written and non-standard conforming in the first place.**_

# Running the code
**By default it runs on 6 processors so run the code with `mpirun -np 6`, if you are not running on 6 you must adjust adjust nx and ny as specified later; also note at least at NCCS or NAS this must be run on a compute node. Finally if the input or output is on a cubed-sphere grid you must run with at least 6.**

If both the input and output are lat-lon or tripolar, it can be run on a single processor. In that case you must explicitly pass nx/ny to the program to override the defaults. **Also note if you specify NX and NY,  NX*NY must equals the number of cores used in the mpirun command.** This could be useful is you need higher speed or are regridding large input or output grids. Finally note that NY must be divisible by 6 if the input or output is on the cube. 

**The minimum arguments are -i, -o, -ogrid and if you use only use these 3 the program must be run with 6 mpi tasks**

# Command line options
Note options have been added as the program has grown beyond the initial command line options. Additional options that have been added or potentially enhanced and the version they were made available will be noted.
* -i input file (at **v2.11.0** and above you can give a list of comma separated files (no spaces!) to regrid. These should all be on the same grid and have the same variables)
* -o output file (at **v2.11.0** and above if specifying multiple input files then correspondingly specify multiple output files, comma separated, no spaces)
* -ogrid encoding of the output grid name, see section on grid names
* -nx x decomposition to use for the decomposition of the target grid
* -ny y decomposition to use for the decomposition of the target grid
* -t date and time to select if the file contains multiple time slices (for example 20000415 210000)
* -method regridding method, the available options are defined here and follow what one can specify in History: https://github.com/GEOS-ESM/MAPL/wiki/Regridding-Methods-Available-In-MAPL#specifying-regridding-methods-in-extdata-and-history-in-mapl-v2220-and-greater. These correspond to the underlying ESMF regridding methods. For more information about the ESMF regridding methods see this document: https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node3.html#SECTION03023000000000000000
* -vars specify a comma separated (no spaces!) list of variables to regrid that subset of the variables from the input file only
* -tp_in tripolar file for input grid is the input file is on a tripolar gird
* -tp_out tripolar file for output grid if output grid is tripolar
* -lon_range from **v2.9.0** if the output grid is lat-lon specify make grid region on the lon direction by specifying a min and max longitude in degrees separated by a space, i.e. `-lon_range 45 65`
* -lon_range from **v2.9.0** if the output grid is lat-lon specify make grid region on the lat direction by specifying a min and max latitude in degrees separated by a space, i.e. `-lat_range 45 65`
* -deflate from **v2.3.2** apply compression to the output file values can be 1 to 9 corresponding to the netcdf deflation level. Default is not compression.
* -shave from **v2.3.2** bit shave the output by specifying the number of bits in the mantissa to retain (a floating point single precision number has 23 bits). Default no bit shaving
* -file_weights from **v2.47.0** if this option is present, when it generates the regridding weights, it will either look for a file of the right name with weights or if said file already exists will use. This provides huge speedup if the file is there since it does not need to recompute the weights. See other notes section for more info.

# Grid Names
The grid name used in ogrid follows the following conventions:
* For lat-lon grid it will be of the form PLEim_worldxjm_world-DATELINE (i.e. PC360x181-DC). In the case of a global lat-lon grid pole is either PC or PE (pole centered or pole edge) and dateline is DE,DC,GC,GE (dateline edge, dateline center, Grenwich center, Grenwich edge). IM_WORLD and JM_WORLD are the number of grid points in the lon and lat direction. From **v2.9.0** onward a regional lat-lon grid can be specified with the -lat_range and -lon_range option. Note you can specify 1 of these or both. Which ever one you specify, set the POLE or DATELINE (or both!) to XY. So a if you want a 180x90 regional grid from 0 to 90 in longitude and -30 to 30 in latitude use these arguments -lat_range -30,30 -lon_range 0,90 -ogrid XY180x90-XY
* For cubed sphere the name will look like PEcube_sizexcubesize*6-CF (i.e. PE180x1080-CF for a c180 cubed sphere grid)
* For tripolar it will look like PE720x410-TM, however you must supply an file containing the tripolar grid coordinates in the correct form

# Other Notes
* The regridding layer we have built on top of ESMF that this code uses assumes the undefined value is that used by MAPL which is 1.0e15 (MAPL_UNDEF constant in the code). Input points that are MAPL_UNDEF do not contribute and any weights involving these points are renormalized. In other words, another undefined value is not supported.
* If you do want to use more than 6 cores and specify an NX and NY note if going to a cubed-sphere output grid NY must be divisible by six (the cubed is decomposed such that each face has NX*NY/6 points).
* This code in no way shape or form supports regridding of the vertical coordinate nor will it for the foreseeable future without extensive development of the MAPL library which is currently not being pursued!
* If using the bit shaving option be careful, this helps with compression but you are throwing away information. If you have fields that varies in the last bits of the mantissa with the same exponent you lose that variation.
* If you are using a modern enough MAPL and need to regrid a whole bunch of files from/to the same grid, I strongly recommend you use the -file_weights option. This will if not found, it will write the regridding weights to a file, and subsequent executions of the code will look for this file and use if found. This will provide speedup since it need not recompute the weights. Note these weights are not transferable. They are for whatever input and output grids were used on the core count used when it was generated so you cannot change that on subsequent executions. When using this option, on the first time used, you will see a new file appear that looks something like this, the import thing is that it start with rh_, e.g. `rh_05760x34560_00005x00024_00360x00181_00005x00024_method_01`. Basically it encodes the input/output grid resolution and layout and method in the file name for the weights.


# Content

- [1 General function of ExtData](#1-general-function-of-extdata)
- [2 Configuration File](#2-configuration-file)
  * [2.1 Configuration File List](#21-configuration-file-list)
  * [2.2 Collections Section](#22-collections-section)
  * [2.3 Samplings](#23-samplings)
  * [2.4 Exports](#24-exports)
    + [2.4.1 Specifying Multiple Rules for a Single Key](#241-specifying-multiple-rules)
  * [4.5 Derived Rules](#25-derived-rules)
    + [4.5.1 Mask Functions](#251-mask-functions)
    + [4.5.2 Arithmetic Functions](#252-arithmetic-functions)
  * [4.6 Example file](#26-example-file)
  * [4.7 Special Cases](#27-special-cases)

# 1 General function of ExtData
`ExtData` is a MAPL component that can provide data from external files. The component receives a list of fields. It then has a list of rules to fill those fields from a time varying sequence of files which is assumes contains no gaps. Each time it runs, based on the rules it first checks if the user wants to even try to update the field at the current time, if so it fills it generally either from the last value in the past or interpolates to the current time, transforming the external data to the application grid if needed. It also has options in cases when the time may fall outside of the dataset such as persisting the closest value either in the future or past or using the closest FULL year of data as a climatology.

# 2 Configuration File
The input file is currently defaulted to be "extdata.yaml". The input file for `ExtDataNG` relies on the YAML format, using the `yaFyaml` library for parsing in Fortran. It is recommended that the users of this version of `ExtData` take a little time to familiarize themselves with YAML syntax. The `yaFyaml` parser supports both flow and block styles. Note that many of the keywords are optional. If so the that will be noted along with the default in this document. Finally any options that represent time or time intervals will use the `ISO 8601` time standard. For times this is something like `2000-01-01T21:00:00` and for time duration this is something like `P1Y1M1DT1D1M0S`. The input format consists of the following sections.

## 2.1 Configuration File List
The input file may contain a list of other files that may be specified with the `subconfigs` keyword that specifies a list.

```
subconfigs: [file1, file2, file3] 
```

or

```
subconfigs:
   - file1
   - file2
   - file3
```

Each file is parsed recursively to **single** dictionaries of sampling parameters, collections and exports internally. This is merely there for the convenience of the user to avoid the problems with a single monolithic file. One thing to keep in mind then is that if you have **ANY DUPLICATE/IDENTICAL KEY NAME IN ANY OF THE FILES THAT GET PARSED IS AN ERROR**.

## 2.2 Collections Section
This section defines a list of datasets that can be used by the rules. This allows multiple rules to use the dataset. By dataset we mean a file or files representing a time series of data. **In addition, the user may specify a valid time range for the files. This is currently necessary to either extrapolate or persist data outside of the dataset as the the component needs to know this information to make decisions. In the future this functionality to detect this may be added but since this requires disk access it is faster if you the user just tells it, after all you took the time to make the data so you had to know this in the first place!** 


If you don't specify the valid range, `ExtData` will try to find a file near the current time using what information can be obtained from just the units of the file frequency (but not the value). For example if your file template has something like `mytemplate_%y4%d2.nc4`, without any other information all it can determine is that you **POSSIBLY** have a file for each day, but not for sure that you have one for each date and certainly the code can not know for how many days you have a file without literally inquiring about every possible file name. The bottom line is, the free times are over; if you have a dataset and want to use data from it when your application is outside of the dataset range, you have to tell it what the range is. **The only exception is the simple case that your template has no tokens in it.** Other than that you had better provide this information, if you don't and try to do anything outside the dataset it will crash!

```
Collections:
   dataset1:
      template: character string
      ref_time: optional, character string
      freq: optional, character string
      valid_range: optional, character string
```

* `template` - grads style character string specifying the file template path for the dataset 
* `ref_time` - optional ISO time. This is used in conjunction with file frequency if the dataset frequency and reference time from that can't be specified by the application start time + the tokens in the template. By default this is the start time of the application.
* `freq` - optional ISO time duration used to specify frequency of the dataset. By default this is "guessed" from the file template based on the right most token.
* `valid_range` - character string of form "IOS time 1/ISO time 2" specify the valid range of times for the dataset. You are telling the application that you should be able to find a valid file on disk by applying any time between that range to the template (within the constraints of the reference time and reference frequency of course). This does not mean you have to use all the data, this is simply telling you what is available. **This is only needed if you need to perform some option that extrapolates outside of the range of the data**. Right now this can be detected in limited cases (no tokens in the template), detection in general is yet to be implemented. Obviously could be expensive as there is really no way but brute forcing this without help from the user.

## 2.3 Samplings
The rules sections consists of a key/value mapping where the key is a label referred to by the export and the value is the list of options that relate to temporal handling of the data. This controls the following behaviors; when to update the Export, whether to interpolate to the current time or persist the last value from the dataset in the past, and what to do if the current time lies outside range of the data (either because of the dataset definition or the source time, also note to make use of this option the user must provide information about the timespan of the dataset, see collection section). By default the field will be updated every time `ExtData`'s run method is called, data will be interpolated to the current time, and what to do if the current time lies outside the dataset (either because of the time range defined for the data or BECAUSE THE USER DID NOT PROVIDE SO WE CAN NOT KNOW WHAT THE IS THE VALID RANGE). If the user wishes for the field to be updated at some other frequency this can be accomplished with the `udpate_reference_time` and `update_frequency` keywords. The user can also provide an offset applied to the current time when making decisions in the `ExtData` run method. Finally the user also can set a source time to restrict usage of the dataset to a smaller window. They may want to do this, for example to reproduce a forecast, where you want to persist the data past a date even if you now have data. Or maybe you have a mutliyear dataset and you want to treat a single year as a climatology.

To summarize the following keywords, extrapolation and source_time define HOW to sample when outside the dataset and the other four involve WHEN to sample.
```
Samplings:
   sample_label:
      extrapolation: optional, character string
      source_time: optional, character string
      time_interpolation: optional, logical
      update_reference_time: optional, character string
      update_frequency: optional, character string
      update_offset: optional, character string
      exact: optional, logical      
```

* `extrapolation` - how to handle extrapolation outside of dataset, options "none" (default, if no data found that is bounded by the dataset fail), "clim" (treat first or last FULL year of the dataset as a climatology), "persist_closest" (simply persist the closest value if outside of the dataset, obviously this turns off time interpolation)
* `time_interpolation` - logical, apply time interpolation (default true) or if false, persist last value in the past
* `update_reference_time` - Reference time used in conjunction with frequency to determine when the pointer is updated, by default this is the application start time.
* `update_frequency` - ISO time duration, the frequency that the pointer will be updated, default is every time ExtData runs
* `update_offset` - ISO time duration representing an offset applied to the current time when updating the pointer. Note the offset has NO effect on WHEN the pointer gets updated. This is an offset applied to the current time once it has been decided that it is time to update the pointer.
* `source_time` - restrict usage of dataset to this time range specified as "IOS time 1/ISO time 2", if outside of this range use the rules for extrapolation outside of dataset. Example use would be to use a single year from a multiple year dataset as climatology. Obviously this must be a subset of the valid range of data in the file. Another obvious use is to reproduce a forecast (maybe now you have data, but what to simulate the time you did not have it)
* `exact` - new from v2.32.0, when filling the field in the file interpolate step, only use the data if the time is exactly the time the data exists on disk, otherwise set the field to MAPL_UNDEF. Note this is different from setting time_interpolation to false, as if you set time_interpolation to false, it just sets the value to the last bracket in the past where as this is more draconian and sets it to data you should not use.

## 2.4 Exports
The rule consists of a key/value mapping where the key is the name of the import to be filled (`variable_name_in_field`) and the value is the list of options for that key.

```
Exports:
   variable_name_in_field:
      collection: character string
      variable: character string
      linear_transformation: optional, list of 2 real number
      regrid: optional, character string
      sample: either sample label or map with sampling options, optional
      fail_on_missing_file: optional, logical
```

* `collection` - name of the dataset to use, can be `/dev/null` which sets the field to zero
* `variable` - name of the variable in the dataset
* `linear_transformation` - shift and scale parameters to apply to the field i.e. `[1.0,2.0]` default none, the sample shown for each value in the field being filled would be like this `output(i,j)=1.0+2.0*output(i,j)`, finally note if you set the collection to `dev/null` the linear transformation is still applied (i.e. if you want to set to something other than zero, so only the shift matters)
* `regrid` - regridding method, options "BILINEAR", "CONSERVE", "VOTE", "FRACTION;value", default bilinear
* `sample` - this is either one of the keys in the Sampling map or you can inline the sampling options. I.E. `sample: sample1` or `sample: {time_interpolation: false}`. This is optional, if not provided uses the defaults for all.
* `fail_on_missing_file` - new from v3.32.0, basically this says, if you are still accessing a dataset in the "normal" mode, not as a climatology or any sort of outside the data persistence, then if a file in the sequence is "missing", i.e. the next file in the sequence can't be found, the bracket is set to MAPL undef rather than just crashing and ExtData just continues on.

Vector handling - sometimes `ExtData` might get a pair of fields that represent of vector pair and should be treated as such when regridding for example. This can be specified using a rule like:

```
Exports:
  U;V: {collection: dataset1, variable: U;V}
```

The key is to put the two components in a single rule with the 2 names separated by a semi-colon (`;`), likewise with the file_var. These are broken apart during parsing but when regridding both components will be treated as a vector, the first variable being treated as the east-west component.

### 2.4.1 Specifying Multiple Rules for a Single Key
Sometimes there may be situations where the user may want to use different Export rules for a variable during different time periods. For example maybe you have real-time data for a period but outside of that you want to use a climatology. Or you just have different datasets for different periods. `ExtData` allows the user to specify multiple rules for a single item. In this way the user may adjust any or all of the parameters for an Export rule. In order to use this feature the user must do two things. 

1. Identify the time you want each rule to start to be applied
2. Any collection referenced using this feature must include a valid time range.

Consider the example:

```
  BC_AIRCRAFT:
    collection: CA2G_BC-em-AIR-anthro_input4MIPs_emissions_CMIP_CEDS-2021-04-21_gn__aviation.x576_y361_z72_t12.%y4.nc4
    regrid: CONSERVE
    sample: CA2G_sample_1
    variable: bc_aviation
  BC_BIOMASS:
    - {starting: "2014-12-01T12:00", collection: CA2G_qfed2.emis_bc.006.%y4%m2%d2.nc4, linear_transformation: [0.0, 0.6], regrid: CONSERVE, sample: CA2G_sample_1, variable: biomass}
    - {starting: "2021-11-01T12:00", collection: CA2G_qfed2.emis_bc.061.%y4%m2%d2.nc4, linear_transformation: [0.0, 0.7], regrid: CONSERVE, sample: CA2G_sample_1, variable: biomass}
```

The setting for `BC_AIRCRAFT` uses a normal rule as explained before. However, the value of the key `BC_BIOMASS` is a sequence. The values of the sequence are normal mappings that define the export rule. Noticed though that each export rule has an extra item "starting". This says that starting on this date use that rule in this example between `2014-12-01T12:00` and `2021-11-01T12:00` it will use that first rule, then at `2021-11-01T12:00` it will use the second rule for any time after that. Note in this example we have changed both the collection we are using and the scaling factors.

## 2.5 Derived Rules
The derived entries consist of a key for the variable name to fill and two elements of the map associated with the key. Derived entries are exactly that. They are derived via some expression from variables in the primary entries. Note that the configuration of the application need not actually need the variables in the expression. If the variable is not needed `ExtData` will add it to the list of primary exports that it must fill. In this case `ExtData` will add the required variables as extra primary exports and will "borrow" the grid (both the horizontal and verical) from the derived export.

```
Derived:
   variable_name_in_field:
      expression: character string
      sample: either sample label or map with sampling options, optional
```

The allowed expressions for the derived export fall into two categories: mask functions, and arbitary functions that use the arithmetic expression parser in MAPL

### 2.5.1 Mask Functions
The first kind of functions are masks. Three types are supported: 
- **zone masking**: masks out anything outside of the min/max latitude defined in the function.
- **region masking**: the user requests the field to be masked with another field. It is assumed all the data points in the masking field are integers and the user says anywhere the mask variable is not one of the integers mask it out. 
 - **box masking**: allows one to specify a box in lat/lon space and anything outside the box is masked.
 

The example below shows how the three masks are used:

```
Derived:
   VARM1: {function: "regionmask(VAR2D,mymask;4,10)"}
   VARM2: {function: "zonemask(VAR2D,-60.1,60.1)"}
   VARM3: {function: "boxmask(VAR2D,-60,60,-60,60)"}
```

Note that the longitudes and latitudes are expressed in degrees. In all 3 masks it is assumed that both VAR2D and mymask are the names of Exports that are also defined.

### 2.5.2 Arithmetic Functions
The other choice is to define the function to be some arithmetic function of some variables that correspond to Exports. For example you could do this:

```
Derived:
   VARM1: {function: "MYVAR_1+(MYVAR_2)*2.0"}
```

For more information about the allowed expressions, see the [MAPL Arithmetic Parser](https://github.com/GEOS-ESM/MAPL/wiki/MAPL-Arithmetic-Parser) document.

## 2.6 Example file
Here is an example input file:
```
Samplings:
  daily_noclim: {update_reference_time: "0" ,update_frequency: PT24H, update_offset: PT12H}
  daily_clim:   {update_reference_time: "0" ,update_frequency: PT24H, update_offset: PT12H, extrapolation: clim}
  persist:      {extrapolation: persist_closet}

Collections:
  qfed_su:           {template: $(PIESA)/sfc/QFED/v2.4r6/Y%y4/M%m2/qfed2.emis_so2.005.%y4%m2%d2.nc4}
  anthro_energy:     {template: $(MERRA2)/sfc/edgar-v42.emis_so2.energy.x1152_y721.19700703T12z_20200703T00z.nc4}
  anthro_non_energy: {template: $(MERRA2)/sfc/edgar-v42.emis_so2.non_energy.x1152_y721.19700703T12z_20200703T00z.nc4 }
  ship_so2:          {template: $(MERRA2)/sfc/edgar-v41.emis_so2.navigation.x360_y181_t47.19750703T12z_20210703T00z.nc4}
  ship_so4:          {template: $(MERRA2)/sfc/edgar-v41.emis_so4.navigation.x360_y181_t47.19750703T12z_20210703T00z.nc4}  
  aircraft_su:       {template: $(AeroCom)/L72/AeroCom.aircraft_fuel.eta.x288_y181_z72_t14.%y4.nc}
  mixing_su:         {template: $(MERRA2)/L72/gmi_ctm_hindcast.oxidants.x144_y91_z72_t14.%y4.nc4}
  dms_su:            {template: $(MERRA2)/sfc/DMSclim_sfcconcentration.x360_y181_t12.Lana2011.nc4}
  mask:              {template: $(AeroCom)/sfc/ARCTAS.region_mask.x540_y361.2008.nc}

Exports:
  SU_BIOMASS:      {collection: qfed_su,           variable: biomass,        sample: daily_noclim, regrid: CONSERVE, linear_transformation: [0., 2.0]}
  SU_ANTHROL1:     {collection: anthro_non_energy, variable: anthrol1,       sample: daily_noclim, regrid: CONSERVE}
  SU_ANTHROL2:     {collection: anthro_energy,     variable: sanl2,          sample: daily_noclim, regrid: CONSERVE}
  SU_SHIPSO2:      {collection: ship_so2,          variable: so2_ship,       sample: daily_noclim, regrid: CONSERVE}
  SU_SHIPSO4:      {collection: ship_so4,          variable: so4_ship,       sample: daily_noclim, regrid: CONSERVE}
  SU_AIRCRAFT:     {collection: aircraft_su,       variable: fuel,           sample: daily_noclim, regrid: CONSERVE}
  SU_DMSO:         {collection: dms_su,            variable: conc,           sample: daily_clim,   regrid: CONSERVE}
  SU_H2O2:         {collection: mixing_su,         variable: H2O2,           sample: daily_noclim, regrid: CONSERVE}
  SU_OH:           {collection: mixing_su,         variable: OH,             sample: daily_noclim, regrid: CONSERVE}
  SU_NO3:          {collection: mixing_su,         variable: NO3,            sample: daily_noclim, regrid: CONSERVE}
  SU_AVIATION_LTO: {collection: /dev/null,         linear_transformation: [0.1, 0.0]}
  SU_AVIATION_CDS: {collection: /dev/null}
  SU_AVIATION_CRS: {collection: /dev/null}
  pSO2_OCS:        {collection: /dev/null}
  SU_regionMask:   {collection: mask,              variable: REGION_MASK,    sample: {extrapolation: persist_closet}}
```

## 2.7 Special Cases
Here, we will touch on some "special" cases that may not be obvious. 

* Time Invariant Data Collections: A scenario is for you to have something like a region mask that does not vary in time, so you will have a single file with no tokens in the template and a single time. This can be easily handled by simply setting the extrapolation keyword to `persist_closest`. You do not need to specify an information like a valid time range or source time. In this trivial case it is smart enough to realize what the valid range is. And by setting the interpolation to `persist_closet`, it will just use the closet value, which is the only value!
* Tile fields: As MAPL v2.40 ExtData2G can now fill fields that are on MAPL tiles if supplied with a file in the tile format which has a single non-time dimension named `tile_index` and a time dimension. This will allow gridded components that live on tiles (basically anything below GEOS_SurfaceGridComp) and currently use MAPL_ReadForcing to transition to using NetCDF files via ExtData. Unlike gridded input that can be spatial transformed, the tile data cannot, as this is simple something we currently cannot do period with tile data. This is not some limitation of ExtData but rather MAPL as a whole. So the file must be the correct file for the fields you intend to fill. So unlike the gridded data where we can have one set of data on a horizontal resolution that can be regridded, for each different model configuration you would need to specify the right tile data. All ExtData does is read the tiles and distribute them according to the attached mask on the grid. This mask is of course determined by how the tiles are "attached" the atmosphere or ocean grid in GEOS. 

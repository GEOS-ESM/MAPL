
## The `ExtData` Gridded Component

`ExtData` stands for "External Data" and is an internal MAPL gridded component 
used to read data (gridded, geospatial) from netCDF files (external data files) on disk. 
`ExtData` is the component of last resort providing fields to existing components
when no other component cannot satisfy the import state requirement.
More specifically, `ExtData` populates fields in the "Import" states within the MAPL hierarchy. 
Only fields designated as part of a component's import state can be filled with `ExtData`.

`ExtData` is instantiated and all its registered methods (`Initialize`, `Run` and `Finalize`)
 are run automatically by the `CAP`.
In a MAPL application, fields added to the Import state of a component 
are passed up the MAPL hierarchy looking for a connectivity to another component 
that will provide data to fill the import. 
If a connectivity is not found these fields will eventually reach the `CAP`. 
At this point any fields that have not have their connectivity satisfied,
 are passed to the `ExtData` through its Export state. 
`ExtData` is in essence a provider of last resort for Import fields that need to be filled with data. 
Like other components, it has a `Run` method that gets called every step in your MAPL application. 
What actually happens when it is run is determined by a `ExtData` resource file. 
`ExtData` can be seen as a a centralized component providing external, time-varying data 
to MAPL components such as chemical and aerosol emissions and forcings like sea surface temperature.

The behavior of `ExtData` is is controlled through a YAML configuration file `extdata.yaml`.
The main goal of the file is to provide a connection between a field name (within the code)
and a variable name in a NetCDF file on disk.
`ExtData` receives a list of fields that need to be supplied with data and parses `extdata.yaml`
to determine if it can supply a variable of that name. 
`extdata.yaml` should be viewed as an announcement of what `ExtData` can provide, 
not what it necessarily will provide. 
In addition to simply announcing what `ExtData` can provide, the user can specify other information 
such as how frequently to update the data from disk and how the data is organized.
This update could be at every step, just once when starting the model run, 
or at a particular time each days. 
It also allows tremendous flexibility as to how the user chooses to organize the data files. 
`ExtData` also allows data to be shifted, scaled, and control what method is used to regrid 
the file to the application grid.

The file `extdata.yaml` allows several following settings that are used by `ExtData` to perform operations
(such as appropriate file selection, horizontal interpolation, time interpolation, etc.) on the fly.
To have additional information on `ExtData`, you may to consult:

[ExtData Next Generation User Guide](https://github.com/GEOS-ESM/MAPL/wiki/ExtData-Next-Generation---User-Guide)


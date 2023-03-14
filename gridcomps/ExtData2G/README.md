
## The `ExtData` Gridded Component

`ExtData` is an ESMF gridded component provided by MAPL to encapsulate access to geospatial data residing on disk. 
It provides a flexible, run-time configurable mechanism for intepolating in time and regridding to arbitrary ESMF grids.
`ExtData` acts as the "provider of last resort" for any fields in model import states that have not been satisfied by the usual MAPL connection rules.


`ExtData` is instantiated and all its registered methods (`Initialize`, `Run` and `Finalize`)
 are run automatically by the `CapGridComp`.
In a MAPL application, fields added to the import state of a component 
are propagated up the MAPL hierarchy until connected to an export state item by some ancestor component.
When no such connection is made, a field will eventually reach the `CapGridComp`, and is passed to 
the `ExtData` gridded component for servicing.
`ExtData` is in essence a provider of last resort for Import fields that need to be filled with data. 
Like other components, it has a `Run` method that gets called every step in your MAPL application. 
What actually happens when it is run is determined by a `ExtData` resource file. 
`ExtData` can be seen as a a centralized component providing external, time-varying data 
to MAPL components such as chemical and aerosol emissions and forcings like sea surface temperature.

The behavior of `ExtData` is is controlled through a YAML configuration file `extdata.yaml`.
The main goal of the file is to provide a connection between a field name (within the code)
and a variable name within a "collection" of NetCDF files.
`ExtData` analyzes each of the fields passed from `CapGridComp` and parses `extdata.yaml`
to determine if it can supply appropriate data. 
`extdata.yaml` should be viewed as an description of what `ExtData` can provide, 
*not* what it necessarily will provide. 
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


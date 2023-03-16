
## `PFIO`

`PFIO`, a MAPL subcomponent, is a parallel I/O tool that is designed 
to facilitate the production of model netCDF output files (organized in collections) and 
to efficiently use available resources in a distributed computing environment. 
`PFIO` asynchronously creates output files therefore allowing the model to proceed with 
calculations without waiting for the I/O tasks to be completed. 
This allows the applications to achieve achieve higher effective write speeds, 
and leads to a decrease of the overall model integration time. 
The goal of `PFIO` is for models to spend more time doing calculations instead 
of waiting on I/O activitiies to be done first.

To implement `PFIO` in an application, it is important to note that `PFIO`
handles netCDF files and therefore follows the netCDF steps to read and create files. 
However, the processes in PFIO are simpler because it works only with 
variable names instead of variable identifier (as in netCDF).
When an application is run with `PFIO`, the available nodes (cores) are split into two groups:
the computing nodes (reserved for model calculations) and 
the I/O nodes (used to perform I/O operations only). 

The [History gridded component](https://github.com/GEOS-ESM/MAPL/tree/main/gridcomps/History) 
relies on `PFIO` to create and write files in the netCDF format.
`PFIO` distributes the output files (collections) to the I/O nodes based on the user's 
configuration set at run time from the command line.
There are typically two main command line configurations:
- **Simple Server**: This the basic configuration where the compute nodes and I/O nodes overlap. `PFIO` is set to run the standard-like Message Passing Interface (MPI) root processor configuration (where IO are completed before calculations resume). This default is efficient at low resolution and/or with few file collections.
- **MultiGroupServer Class**: Here, the compute nodes and I/O nodes are completely separate. All the I/O procedures are handled by the I/O nodes that perform their tasks wven while the compute nodes do model integration.

To have additional information on `PFIO`, you may to consult:

[PFIO: a High Performance Client Server I/O Layer](https://github.com/GEOS-ESM/MAPL/wiki/PFIO:-a-High-Performance-Client-Server-I-O-Layer)



## The `History` Gridded Component

`History` is a highly-configurable ESMF gridded component provided my MAPL which is used to manage streams of output data from a MAPL hierarchy.
It is able to write any export item from any component into a specified file collection during the course of a run.    This output is highly configurable, allowing specification of output grid, vertical interpolation, temporal frequency and/or averaging.    The component also supports output of derived quantities which are computed from native fields with a small suite of mathematical operations.

`History` uses [MAPL PFIO](https://github.com/GEOS-ESM/MAPL/wiki/PFIO:-a-High-Performance-Client-Server-I-O-Layer)
 for creating and writing its files in the netCDF format.
Its behavior is controlled through its configuration file, `HISTORY.rc`, which primarily consists of a list
of collections to produced.
Each collection can have the following properties:
- All the fields are on the same grid.
- If fields have vertical levels, all of them should be either at the center or at the edge. We cannot have both in the same collection.
- Its fields may be `instantaneous` or `time-averaged`, but all fields within a collection use the same time discretization.
- A beginning and an end time may be specified for each collection.
- Collections are a set of files with a common name template.
- Files in a collection have a fixed number of time groups in them.
- Data in each time group are `time-stamped`; for time-averaged data, the center of the averaging period is used.
- Files in a collection can have time-templated names. The template values correspond to the times on the first group in the file.

The component has no true export state, since its products are diagnostic file collections.
It does have both import and internal states, which can be treated as in any other MAPL
component, but it generally makes no sense to checkpoint and restart these.

The  main file in the `History` source code is `MAPL_HistoryGridComp.F90`
that contains the `Initialize`, `Run` and `Finalize` methods for `History`.
The three methods are called at the level of CAP.

Additional information about the `History` griddec component can be found at:

[MAPL History Component](https://github.com/GEOS-ESM/MAPL/wiki/MAPL-History-Component)

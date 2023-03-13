
## The `History` Gridded Component

`History` is an internal MAPL gridded component used to manage output streams from a MAPL hierarchy. 
It writes Fields in the Export states of all MAPL components in a hierarchy to file collections 
during the course of a run. 
It also has the some capability to interpolate the fields horizontally and/or vertically before outputing
them.
The main features of `History` are its:
- **Flexibility**: can quickly be integrated in a MAPL application.
- **Configurability**: at run time, users to select which fields to produce, at which grid,
at which frequency, in which collection, at which resolution, etc. A field can also be derived from existing ones.

The component has no true export state, since its products are diagnostic file collections.
It does have both Import and Internal states, which can be treated as in any other MAPL
component, but it generally makes no sense to checkpoint and restart these.

The  main file in the `History` source code is `MAPL_HistoryGridComp.F90`
that contains the `Initialize`, `Run` and `Finalize` methods for `History`.
The three methods are called at the level of CAP.

`History` uses [MAPL PFIO](https://github.com/GEOS-ESM/MAPL/wiki/PFIO:-a-High-Performance-Client-Server-I-O-Layer)
 for creating and writing its files in the netCDF format.
Its behavior is controlled through its configuration file, `HISTORY.rc`, which contains a list
of collections to produce.
Each collection can have the following properties:
- All the fields are on the same grid.
- If fields have vertical levels, all of them should be either at the center or at the edge. We cannot have both in the same collection.
- Its fields may be `instantaneous` or `time-averaged`, but all fields within a collection use the same time discretization.
- A beginning and an end time may be specified for each collection.
- Collections are a set of files with a common name template.
- Files in a collection have a fixed number of time groups in them.
- Data in each time group are `time-stamped`; for time-averaged data, the center of the averaging period is used.
- Files in a collection can have time-templated names. The template values correspond to the times on the first group in the file.


To have additional information on `History`, you may to consult:

[MAPL History Component](https://github.com/GEOS-ESM/MAPL/wiki/MAPL-History-Component)

# dims_z_field

Same shape as `vertical_field` but both sides declare `dims: z`, so the
field has no horizontal dimensions: only the vertical one (rank 1).
Checks that two such items connect directly, with no extension.

The geometry of a `dims: z` item is not compared; that is covered by unit
tests, because the legacy path still compares geoms and so cannot run a
scenario with differing horizontal grids.

There is no run phase, so no value is checked.

Expected: `comp_src` export and `comp_dst` import are complete, R4, rank 1,
and the connection has no chain.

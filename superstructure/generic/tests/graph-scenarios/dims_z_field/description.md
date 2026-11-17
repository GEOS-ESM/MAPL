# dims_z_field

Same shape as `vertical_field` but both sides declare `dims: z`, so the
field has no horizontal dimensions: only the vertical one (rank 1).
Checks that two such items connect directly, with no extension, even when
the two components have different horizontal grids (12x13 and 6x7): the
geometry of a `dims: z` item is not compared.

Each component declares its own `geometry:`, so the cap's geometry is not
used by either child. Neither child declares a `vertical_grid`: both
inherit the cap's, so they share one grid. A fixed-levels grid declared by
each child would get its own id and would not match the other, even with
equal levels (as in legacy), so the pair would need a vertical regrid.

There is no run phase, so no value is checked.

Expected: `comp_src` export and `comp_dst` import are complete, R4, rank 1,
and the connection has no chain.

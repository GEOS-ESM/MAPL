# vertical_field

Same shape as `plain_field` but both sides declare `vertical_dim_spec:
CENTER`, so the field has a vertical dimension (rank 3). Checks that a
matching vertical axis neither falsely mismatches nor needs an extension.

The cap geometry declares a 5-level basic vertical grid. There is no run
phase, so no value is checked.

Expected: `comp_src` export and `comp_dst` import are complete, R4, rank 3.

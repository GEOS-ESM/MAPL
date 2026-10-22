# plain_field

Baseline connection: `comp_src` exports `T`, `comp_dst` imports `T`, with
identical standard name (`A1 standard name`), units (`barn`), typekind (default
R4) and no vertical or ungridded dimension. No extension is needed.

`comp_src` writes `42.` in its run phase (`run: T: [42]`), so the value
check confirms the data reaches `comp_dst`'s import.

Expected: all four items (`comp_src` export, `comp_dst` import, user and
outer views) are complete, R4, rank 2, value `42.`. Also the fixture used
by the restart-metadata and both-mode equivalence tests.

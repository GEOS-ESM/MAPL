# ungridded_dims

Same shape as `plain_field` but both sides declare one ungridded
dimension (`foo1`, extent 3). Checks that equal ungridded dimensions
neither falsely mismatch nor need an extension.

There is no run phase, so no value is checked.

Expected: `comp_src` export and `comp_dst` import are complete, R4,
rank 3 (two horizontal dimensions plus the ungridded one).

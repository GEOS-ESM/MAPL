# bracket_time_interp

`A` (`libproto_bracket_gc`) exports a two-member BRACKET `B`; `B` imports a
FIELD `B` on the same 12x13 lat-lon grid. The only mismatch is the item
shape, so the chain is one `BracketTimeInterpolationTransform` step.

Used by `Test_GraphAddressStability.pf`, which checks that the data address
of `B`'s import stays fixed over several run steps when the graph owns the
user states. Not in the `Test_Scenarios.pf` table.

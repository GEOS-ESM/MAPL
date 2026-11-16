# Shared expression evaluation with per-consumer result units

Component A exports three R4 fields in metres, filled with 1, 2 and 3.
Its expression `(A + B)/C` is declared in metres. Component B imports
the result as R4 in metres; component C imports it as R4 in centimetres.
Their expected values are 1 and 100, respectively.

Both consumers share one R4 evaluation, reading inputs in their own units.
Only C's result chain converts metres to centimetres. This scenario runs
in registry, graph and both modes.

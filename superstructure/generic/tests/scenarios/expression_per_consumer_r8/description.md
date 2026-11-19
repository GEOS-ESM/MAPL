# Expression evaluation at each consumer's precision (pending, graph-only)

Component A exports three R4 fields in metres, filled with 1, 2 and 3.
Its expression `(A + B)/C` is declared in metres. Component B imports
the result as R4 in metres; component C imports it as R8 in centimetres.
Their expected values are 1 and 100, respectively.

The graph evaluates separately at each precision. B's evaluation reads
the inputs directly. C's evaluation reads three precision-converted
inputs, each still in metres, then converts the result to centimetres.

Legacy orders expression extensions TYPEKIND before CLASS
(`superstructure/generic/specs/ExpressionClassAspect.F90:119-124`), so
`superstructure/generic/transforms/CouplerMetaComponent.F90:588` fails
because the precision coupler receives an expression rather than a Field
or FieldBundle. Registry import couplers still run in every mode:
`superstructure/generic/OuterMetaComponent/initialize_user.F90:29-32`
and `superstructure/generic/OuterMetaComponent/run_user.F90:48-52`.

This scenario becomes ready when legacy couplers stop running in graph
mode. Fixing or gating legacy couplers belongs to legacy retirement,
not expression input conversion. The R4 variant exercises per-consumer
result units in all modes without that blocker.

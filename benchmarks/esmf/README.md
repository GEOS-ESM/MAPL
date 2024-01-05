This benchmark is to measure the overhead of running a stub ESMF
GridComp.  It reports the time per call as well as the total time for
1000 such calls.

On an Apple M2 laptop this is showing ~1 microsecond per call using a
debug build of ESMF 8.5.  I.e., tihs is unlikely to have a measurable
performance impact even if a stub coupler is run for every import and
export for every gridcomp.  Total run time would go up by at most 0.01
seconds per time step - well within the noise.

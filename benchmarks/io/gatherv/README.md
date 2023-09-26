This benchmark times the `MPI_Gatherv()` operations used in the
current (2023) generation of GEOS checkpoint operations. 

In the actual model, `MPI_Gatherv` is needed instead of just
`MPI_Gather` because local domains may vary slightly in dimension.  In
this benchmark subdomains are all the same size, but we still use
`MPI_Gatherv` to ensure relevance to GEOS.


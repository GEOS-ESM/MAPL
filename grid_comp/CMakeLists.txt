esma_set_this (OVERRIDE MAPL.grid_comp)

set (srcs
        NUOPC_MAPLcap.F90
        NUOPC_MAPLcap_Class.F90
        NUOPCmap.F90
)

esma_add_library(
        ${this} SRCS ${srcs}
        DEPENDENCIES MAPL.base MAPL.shared MAPL.profiler MAPL.pfio MAPL_cfio_r4 pflogger gftl-shared MPI::MPI_Fortran
)

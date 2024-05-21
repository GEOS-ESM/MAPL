target_sources(MAPL.generic3g PRIVATE

    initialize.F90           
    run.F90
    finalize.F90       
    get_states.F90           
    get_clock.F90      
    set_clock.F90
    run_export_couplers.F90  
    run_import_couplers.F90
    clock_advance.F90  
    new_GriddedComponentDriver.F90 
    get_gridcomp.F90 
    get_name.F90 
    add_export_coupler.F90 
    add_import_coupler.F90

)

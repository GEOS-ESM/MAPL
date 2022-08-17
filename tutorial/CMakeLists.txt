install (PROGRAMS run_tutorial_case.sh DESTINATION bin)

add_subdirectory (driver_app)
add_subdirectory (grid_comps)

file (GLOB_RECURSE tutorial_files CONFIGURE_DEPENDS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} mapl_tutorials/*)
foreach ( file ${tutorial_files} )
   get_filename_component( dir ${file} DIRECTORY )
   install( FILES ${file} DESTINATION etc/${dir} )
endforeach()

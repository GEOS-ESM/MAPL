# MAPL.utilities sources have been moved to MAPL.utils (utils/).
# This stub keeps the MAPL.utilities target alive as an INTERFACE
# library for any CMake consumers not yet updated.
add_library (MAPL.utilities INTERFACE)
target_link_libraries (MAPL.utilities INTERFACE MAPL.utils)
install (TARGETS MAPL.utilities EXPORT MAPL-targets)

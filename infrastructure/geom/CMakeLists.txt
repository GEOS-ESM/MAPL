# Geom infrastructure layer (Tier 3 in MAPL v3 restructuring).
# Consolidates: geom/, GeomIO/, regridder_mgr/.
# Note: MAPL.geom target is still defined in geom/CMakeLists.txt (stub),
# which sources files from geom/ subdirectory here.

add_subdirectory (regridder_mgr)
add_subdirectory (GeomIO)

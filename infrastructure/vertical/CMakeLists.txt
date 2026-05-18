# Vertical infrastructure layer (Tier 3 in MAPL v3 restructuring).
# Consolidates: vertical_grid/, vertical/.
# These remain as separate sub-libraries due to their differing dependency levels.

add_subdirectory (vertical_grid)
add_subdirectory (vertical)

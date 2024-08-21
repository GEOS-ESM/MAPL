target_sources(MAPL.generic3g PRIVATE

  VerticalGrid.F90
  BasicVerticalGrid.F90
  MirrorVerticalGrid.F90
  FixedLevelsVerticalGrid.F90
  ModelVerticalGrid.F90
)

add_subdirectory(BasicVerticalGrid)
add_subdirectory(ModelVerticalGrid)

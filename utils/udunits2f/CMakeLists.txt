find_package(udunits REQUIRED)
find_package(EXPAT REQUIRED)

target_sources(MAPL.utils PRIVATE
  CptrWrapper.F90
  UDSystem.F90
  udunits2f.F90
  encoding.F90
  interfaces.F90
  status_codes.F90
  ut_set_ignore_error_message_handler.c
  )

target_link_libraries(MAPL.utils PUBLIC udunits::udunits)
target_link_libraries(MAPL.utils PUBLIC EXPAT::EXPAT)

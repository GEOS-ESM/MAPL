# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(this pflogger)

set(SRCS
     WrapArray.F90
     pflogger_stub.F90
     )

add_library (${this} STATIC ${SRCS})
add_library(PFLOGGER::pflogger ALIAS pflogger)
  
set (dest "PFLOGGER-${PFLOGGER_VERSION_MAJOR}.${PFLOGGER_VERSION_MINOR}")

target_include_directories(${this} PUBLIC
  $<BUILD_INTERFACE:${PFLOGGER_BINARY_DIR}/include>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${dest}/include>
  )

target_link_libraries(${this} PUBLIC GFTL_SHARED::gftl-shared GFTL::gftl)

install (TARGETS pflogger EXPORT PFLOGGER DESTINATION ${dest}/lib)
install (DIRECTORY  ${PROJECT_BINARY_DIR}/include/ DESTINATION ${dest}/include)
install (FILES  ${PROJECT_SOURCE_DIR}/src/error_handling_macros.fh DESTINATION ${dest}/include)

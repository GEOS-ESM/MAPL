#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.

file (COPY MAPL_GridCompSpecs_ACG.py DESTINATION ${esma_etc}/MAPL)
file (COPY mapl_acg.pl DESTINATION ${esma_etc}/MAPL)
file (COPY mapl_stub.pl DESTINATION ${esma_etc}/MAPL)

install (PROGRAMS
  MAPL_GridCompSpecs_ACG.py
  mapl_acg.pl
  mapl_stub.pl
  # [lrb, 2021-7-22] For GCHP: Replaced TYPE arg with DESTINATION. The TYPE 
  #                            argument wasn't added until CMake 3.14 and our
  #                            current requirement is 3.13.
- # TYPE SYSCONF
  DESTINATION etc
  )

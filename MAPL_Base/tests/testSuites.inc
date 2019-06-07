ADD_TEST_SUITE(DirPath_suite)

ADD_TEST_SUITE(test_MAPL_ConfigMod_suite)
ADD_TEST_SUITE(test_sort_suite)
ADD_TEST_SUITE(Test_SimpleMAPLcomp_mod_suite)

! CFIO Tests
! moved to GMAO_pFIO/tests

! Grids and Regridders
ADD_TEST_SUITE(Test_StringGridFactoryMapMod_suite)
ADD_TEST_SUITE(Test_LatLonGridFactoryMod_suite)
ADD_TEST_SUITE(Test_LatLonCornersMod_suite)
ADD_TEST_SUITE(Test_GridManagerMod_suite)

! The test suite below was recovered from a legacy standalone
! program.  Unfortunately it is not currently working.  Possibly
! it just needs appropriate input files.
!ADD_TEST_SUITE(Test_CFIO_Bundle_mod_suite)


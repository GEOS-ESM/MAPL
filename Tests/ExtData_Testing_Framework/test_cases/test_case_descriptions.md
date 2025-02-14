# Test Case Descriptions

Note all test cases are in a numbered directory caseX, where a X is an integer and each case is described in the following list where the list number X is for folder caseX

To run the test cases you can use the provided script and run the command:
path_to_script/run_extdatadriver_cases.py --builddir path_to_geos_install/bin --casedir path_to_cases --cases cases.txt --savelog TRUE

1. 12-month/12 time 2004 file with 2 updates, non-climatology
2. 12-month/12 time 2004 file with 2 updates, climatology
3. monthly files for 2004 file with 1 updates, climatology for 2007
4. simple everytime update with daily files and no time interpolation
5. simple everytime update with daily files and time interpolation
6. daily files for 0z for a year, fill a time at 12z on the 31st of december 2006 as a climatology
7. 12-month/12 time 2004 file with an update that will wrap around the year
8. simple everytime update with a new file every 3 hours relative to 003000z
9. Single time file, persisted at all times
10. Interpolation outside of data set (Harvard mode), make a multi year dataset. Define as not a climatology and ask for data after dataset time range
11. Interpolation outside of data set (Harvard mode), make a multi year dataset. Define as not a climatology and ask for data before datset time range
12. Test of case where you want to make a really coarse file in History that can not be decomposed on the default layout in the rc file. Be able to output such a file, then read back in on same grid in ExtData
13. Testing that we can take a climatology for a non-leap year and interpolate to a leap year. 12 files each with the midmonth value for 2007 (non-leap year). Interpolate to 02/29/2008 (leap year)
14. Testing that we can take a climatology for a non-leap year and interpolate to a leap year. Daily files each with 1 value for 2007 (non-leap year). Interpolate to 02/29/2008 (leap year)
15. Testing that we can take a climatology for a non-leap year and interpolate to a non-leap year. Daily files each with 1 value for 2007 (non-leap year). Interpolate to 03/29/2006 (leap year)
16. Testing that we can take a climatology for a leap year and interpolate to a non-leap year. Daily files each with 1 value for 2008 (leap year). Interpolate to 03/29/2006 15z (leap year)
17. Test ability of ExtData (2G only) to allow for subconfigs, i.e. split input yaml files into multiple files
18. Test vector regridding
19. Test set file to /dev/null
20. Make daily files for 2016. Then start on February 29th in 2020 and allow extrapolation outside of dataset a climatology
21. Test derived export to create sum of 2 variables
22. Test multiple datasets where an export uses both with with no extrapolation outside and crosses transition date
23. Test multiple datasets and treat Climatology in the first and a real-time in the 2nd
24. Test reading cubed-sphere input
25. Test reading edge variable
26. Test reading edge + center variables
27. Case with a "gap" in the data
28. "Replay" type run, update every time
29. "Replay" type run, update once a day with offset
30. Case1 with deflate compression
31. Case1 with deflate compression and MAPL bit-shaving
32. Case1 with deflate compression and NetCDF bitgroom quantization (only enabled if netcdf built with quantization support)
33. Case1 with deflate compression and NetCDF bitround quantization (only enabled if netcdf built with quantization support)
34. Case1 with deflate compression and NetCDF granular_bitround quantization (only enabled if netcdf built with quantization support)
35. Case1 with zstandard compression (only enabled if netcdf built with zstandard support)
36. Case1 with zstandard compression and NetCDF granular_bitround quantization (only enabled if netcdf built with quantization
    support and zstandard support)
37. Test that when time interpolation is off, missing files is allowed, that the last read value is persisted during the period of the missing file

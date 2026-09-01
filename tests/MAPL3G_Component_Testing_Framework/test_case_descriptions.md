# Test Case Descriptions

Note all test cases are in a numbered directory caseX, where a X is an integer and each case is described in the following list where the list number X is for folder caseX

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
13. Testing that we can take a climatology for a non-leap year and interpolate to a leap year. 12 files each with the midmonth value for 2007 (non-leap year). Interpolate to 02/29/2008 (leap year)
14. Testing that we can take a climatology for a non-leap year and interpolate to a leap year. Daily files each with 1 value for 2007 (non-leap year). Interpolate to 02/29/2008 (leap year)
15. Testing that we can take a climatology for a non-leap year and interpolate to a non-leap year. Daily files each with 1 value for 2007 (non-leap year). Interpolate to 03/29/2006 (leap year)
16. Testing that we can take a climatology for a leap year and interpolate to a non-leap year. Daily files each with 1 value for 2008 (leap year). Interpolate to 03/29/2006 15z (leap year)
17. Test ability of ExtData (2G only) to allow for subconfigs, i.e. split input yaml files into multiple files
18. Test vector regridding
19. Test set file to /dev/null
21. Test derived export to create sum of 2 variables
22. Test multiple rules for an item
23. Test multiple datasets and treat Climatology in the first and a real-time in the 2nd
24. Test ability to read and write cubed-sphere files
30. Case1 with deflate compression
39. Test adding a scaling and offset to an item
40. Test fractional regridding, ensure fractions of c24 cells regridded to c12 add up to 1
41. Test History for ability to output daily averages
42. Test of Historys ability to output monthly and djf averages
43. Test of Historys ability to output daily averge and modify on a per-variable basis the units, averaging type, and precision
44. Test ExtData file-read logging: enable log_files_read in extdata config, run a 2-step case where step 1 generates and writes files and step 2 reads them with logging enabled, then compare the generated log against a golden reference file
45. Test ExtData routing through AsyncInputServer in synchronous-fallback mode; intended as the baseline YAML case for future cluster-side asynchronous input-server experiments
46. Test AsyncInputServer single-slot cache path by routing two identical ExtData slab requests through the same local async reader and checking the duplicate imports remain equal
47. Test AsyncInputServer rolling no-interpolation path across multiple timesteps by keeping duplicate ExtData imports equal while verifying the generated file-read log still collapses to one source file
48. Test AsyncInputServer time-interpolation path across multiple sampled times by routing duplicate imports through the async server and logging which source files are consumed
49. Test AsyncInputServer time-interpolation rollover behavior across a bracket boundary by sampling before and after the day-change interpolation pivot

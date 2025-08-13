# Introduction
The MAPL CapGridComp is a special component in a MAPL hierarchy provided for the convenience of users. It provides two main functions.
- Provide a place "hang" the root component of the MAPL hierarchy off of, as well as drive two service components, MAPL_HISTORY and MAPL_EXTDATA
- Control overall time stepping and clock for the MAPL hierachy.
# CAP.rc File
The CapGridComp is driven by a file named `CAP.rc`. Note that it uses a way to provide a date and time by encoding each in an integer for the form YYYYMMDD HHMMSS. Here is an example showing the options:
```
ROOT_NAME: GCM
ROOT_CF: AGCM.rc
HIST_CF: HISTORY.rc

BEG_DATE:     18910301 000000
END_DATE:     29990302 210000
JOB_SGMT:     00000000 010000
NUM_SGMT:     1
HEARTBEAT_DT:       450

USE_SHMEM: 0
USE_EXTDATA2G: .TRUE.

MAPL_ENABLE_TIMERS: YES
MAPL_ENABLE_MEMUTILS: NO
PRINTSPEC: 0  # (0: OFF, 1: IMPORT & EXPORT, 2: IMPORT, 3: EXPORT)
```
Here are the options explained:
- ROOT_NAME: this is the name that will be used for the root component of the hierarchy
- ROOT_CF: this is the rc file that will be provided to the MAPL hierarchy
- HIST_CF: this the rc file that will be provided to the History component
- BEG_DATE: this is the start time of the ESMF Clock that the CapGridComp will create in packed integer format
- END_DATE: this is the end time of the ESMF Clock that the CapGridComp will create, the CapGridComp will stop execution at this time no-matter what in packed integer format
- JOB_SGMT: this is the duration of the model run in packed integer date time format. In this case the date part is interpreted as a duration.
- NUM_SGMT: this is not actually used by CapGridComp, but other scripts expect it so you might as well put it there
- USE_SHMEM: turn on shared memory option in MAPL layer to make use of the UNIX shared memory.
- USE_EXTDATA2G: logical to control which version of ExtData is used
- MAPL_ENABLE_TIMERS: YES or NO, enables MAPL profiler, really no downside to always leaving on
- MAPL_ENABLE_MEMUTILS: YES or NO, enable some extra memory utilities
- PRINTSPEC: 0, 1, 2, or 3 Option to print the content of each component state in the MAPL hierarchy and stop execution

Tips
- You as a user should almost never have to touch anything in here other than the JOB_SGMT and maybe the END_DATE in day to day use

# cap_restart file
The cap_restart file controls the start time of the model. If this file is not provided, the BEG_DATE from the CAP.rc file is used. The file consists of a single line with two integers that have been packed with the date as follows:

YYYYMMDD HHMMSS

for example to start on November 6th, 2020 at 21Z you would set the cap_restart to:
```
20001106 210000
```

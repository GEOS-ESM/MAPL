# Tutorial 1 - Hello World
Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/hello_world_gridcomp/HelloWorld_GridComp.F90

For this tutorial we will make the simplest possible gridded component we can and have it print hello in it's run method.

The gridded component itself is run from the MAPL "CAP". This is a layer that the user should never have to touch. It's main function as far as the user is concerned is to perform the time stepping controlled via the CAP.rc and run the "root" gridded component (in this example HelloWorld_GridComp.F90) the user or program specified as well as two other special gridded components "History" and "ExtData" that provide services that we will talk about in later tutorials.

# HelloWorld_GridComp.F90 Explanation

If you look in the gridded component you will see that it is quite simple and is just about the minumum lines needed to create a gridded component, a grid for the component, and a run method that does something.

The first routine is the setServices. This is where the user registers the actual methods to be used during the initilze and run phases of the gridded component and are specifed via the SetEntryPoint calls. In addition the MAPL_GenericSetServices is called and every MAPL component must call this before ending the subroutine.

Next we see that a custom initialization routine "my_initialize" is created. It has two calls, the first tells it how to create the grid that is will be used by the gridded component.

MAPL_GridCreate actually examines the components RC file which in this case is "hello_world.rc". The user will notice these lines:
```
hello_world.GRID_TYPE: LatLon
hello_world.GRIDNAME: DC90x45-PC
hello_world.LM: 72
hello_world.IM_WORLD: 90
hello_world.JM_WORLD: 45
hello_world.POLE: 'PC'
hello_world.DATELINE: 'DC'
```
Generally the user will not have to modify these are the setup scripts when running the model would define this for you. In this case it is saying the grid will be a 90x45 lat-lon grid with LM vertical levels.

After this call MAPL_GenericInitialize is called. Once again every custom initialize routine must call this. If no custom initialize routine is defined this will be call automatically.

Finally we get to the run method my_run. This was registered and will be executed each time step. As you can see if does very little in this example. It gets the current time from the clock (this literally a clock that is advanced by the MAPL "CAP"), then prints the obligatory "Hello World" and finally uses an ESMF call to print the current time.

# Running the code
When you run the code the first few lines will look like this:
```
srun: cluster configuration lacks support for cpu binding
      MAPL: No configure file specified for logging layer.  Using defaults.
 Starting pFIO input server on Clients
 Starting pFIO output server on Clients
      MAPL: Running with MOAB library for ESMF Mesh: F
     SHMEM: NumCores per Node = 1
     SHMEM: NumNodes in use   = 1
     SHMEM: Total PEs         = 1
     SHMEM: NumNodes in use  = 1
 Integer*4 Resource Parameter: HEARTBEAT_DT:3600
 NOT using buffer I/O for file: cap_restart
       CAP: Read CAP restart properly, Current Date =   2007/08/01
       CAP:                            Current Time =   00/00/00
 Character Resource Parameter: ROOT_CF:hello_world.rc
 Character Resource Parameter: ROOT_NAME:hello_world
 Character Resource Parameter: HIST_CF:HISTORY.rc
  oserver is not split

 EXPSRC:
 EXPID:
 Descr:
 DisableSubVmChecks: F
 BlockSize:           10
 MarkDone:             0
 PrePost:              1

 Reading HISTORY RC Files:
 -------------------------


 Hello World, I say the time is:
Time -----------------------------------
2007-08-01T00:00:00
end Time -------------------------------

 AGCM Date: 2007/08/01  Time: 01:00:00  Throughput(days/day)[Avg Tot Run]:     407962.2     410922.5   18590964.7  TimeRemaining(Est) 000:00:00    2.7% :  13.3% Mem Comm:Used


 Hello World, I say the time is:
Time -----------------------------------
2007-08-01T01:00:00
end Time -------------------------------

 AGCM Date: 2007/08/01  Time: 02:00:00  Throughput(days/day)[Avg Tot Run]:   21061906.1   10684057.3   25508595.8  TimeRemaining(Est) 000:00:00    2.7% :  13.3% Mem Comm:Used
 ```
 Lets see how this corresponds to what is in the input files. 

 First lets discuss the CAP.rc, the relevant lines are
 ```
 JOB_SGMT:     00000001 000000
HEARTBEAT_DT: 3600
```
which tell the MAPL "CAP" to run 1 day via the JOB_SGMT line and with a timestep of 3600s. In addition the 
```
ROOT_CF: hello_world.rc
```
tells "CAP" that the root component will use hello_world.rc.
Finally you will notice that hello_world.rc has these lines:
```
NX: 1
NY: 1
```
This says we will be using decomposing each dimension of the grid by 1 (so no decomposition at all!). A rule of thumb, the number of MPI tasks must be equal to NX*NY.

Now to connect this to the output. We see the that it reports
```
SHMEM: Total PEs = 1
```
which says we are using 1 MPI task.
Then later you the tell works and quick glance should confirm it is stepping the clock by 1 hour each time. Finally you see lines like this:
``` 
AGCM Date: 2007/08/01  Time: 02:00:00  Throughput(days/day)[Avg Tot Run]:   21061906.1   10684057.3   25508595.8  TimeRemaining(Est) 000:00:00    2.7% :  13.3% Mem Comm:Used 
```
This is actually reported by the "CAP" itself. and prints the current time as well as some statistics about memroy use and throughput. The astute user will notice that the time reported here is 1 hour after the time printed in the gridded component. This is because the clock is advanced at the end of each iteration in the "CAP", after the component is run and this reporting is at the very end of each iteration.

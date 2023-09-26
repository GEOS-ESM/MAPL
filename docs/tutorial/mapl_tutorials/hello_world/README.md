# Tutorial 1 - Hello World
Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/hello_world_gridcomp/HelloWorld_GridComp.F90

For this tutorial we will make the simplest possible gridded component we can and have it print hello in its run method.

The gridded component itself is run from the MAPL "CAP". This is a layer that the user should never have to touch. Its main function, as far as the user is concerned, is to perform the time stepping controlled via the CAP.rc and run the "root" gridded component (in this example HelloWorld_GridComp.F90) the user or program specified, as well as two other special gridded components, "History" and "ExtData", that provide services that we will talk about in later tutorials.

# HelloWorld_GridComp.F90 Explanation

If you look in the gridded component you will see that it is quite simple and is just about the minumum lines needed to create a gridded component, a grid for the component, and a run method that does something.

The first routine is the setServices. This is the ONLY routine in the module that should be public. Everything else should be private. In addition the SetServices interface must match the interface defined by ESMF>  The main function of the SetServies is to let the user registers the actual methods to be used during the initialize and run phases of the gridded component. These specifed via the SetEntryPoint calls and methods defined in the same module. They also must be defined with the interface prescribed by ESMF. In addition, the MAPL_GenericSetServices is called in this routine and every MAPL component must call this before ending the subroutine. The MAPL_GenericSetServices handles all the extra services provided by MAPL beyond EMSF.

Next we see that a custom initialization routine "my_initialize" is created. Notice the subroutine interface. This is the interface all initialize, run, and finalize methods registered my ESMF SetEntryPoint methods must follow. The import state contains all the fields (as well as possibly other types) that will be needed to run the component. The component should not modify the import state. Likewise the export state is what the gridded component produces for use by other components. Finally the clock is just that, a clock that defines the current temporal situation.

In this exmaple, the initialize routine only has two calls. The first tells it how to create the grid that is will be used by the gridded component.

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

After this call MAPL_GenericInitialize is called. This is again a MAPL call that handles all the MAPL specify functionality. It also calls the initialize methods of any child, which will be discussed subsequent tutorials. Once again every custom initialize routine must call this. If no custom initialize routine is defined this will be called automatically.

Finally we get to the run method my_run. Notice it has the same interface the initialize method. This was registered and will be executed each time step. As you can see if does very little in this example. It gets the current time from the ESMF clock (this literally a clock that is advanced by the MAPL "CAP"). The time is stored in a variable of `type(ESMF_Time)` declared in the subroutine.  It then prints the obligatory "Hello World" and finally uses an ESMF cal which takes an ESMF time and prints it as a string.

# A Note on Error Handling
You will notice that the setServices, initialize, and run subroutines all have an optional rc return variable. This is represents a return code that the calling routine can check to see if the subroutine executed successfully or produced an error.  All ESMF and MAPL subroutines and functions have an optional rc value that can be checked when making a call. To check the return status you would do something like this. 
```
integer :: status


call ESMF_Foo(arg1,arg2,rc=status)
if (status/=ESMF_SUCCESS) then
   if present(rc)) then
      rc =status
      write(*,*)"Error ",rc," in ",__FILE," on ",__LINE__
      return
   end if
end
```

This would get very tedious, not to mention make the code hard to read if the user had to do this after every subroutine or function call. To assist the developer MAPL defines a collection of preprocessor macros for error checking .

You will notice that all subroutine calls in this example end with `_RC`. This is a preprocessor macro that expands to `rc=status); _VERIFY(status`. 

`_VERIFY` itself is another macro that essentially implements the lines after the call to `ESMF_Foo` in the previous example. It will check the status and if there is an error report the file and line and return.

At the end of each subroutine you will notice another macro, `_RETURN(_SUCCESS)`. This macro ensures that if the optional rc code is passed, it will be set to the "succes" value if the caller is checking the return code. It general placed at the very end of a subroutine.

All new functions and subroutines should have an optional rc code and use these macros. It will make debugging and crash analysis much easier.

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

Finally in you should see a "cap_restart" file in the run directory. This is the time the application will actually start at. It must be equal or later than the BEG_DATE  in the CAP.rc and before the END_DATE. Note that generally these are only needed when running real experiments with the model. One final note about the "cap_restart". When the application finishes it overwrites the cap_restart with the final application time.

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

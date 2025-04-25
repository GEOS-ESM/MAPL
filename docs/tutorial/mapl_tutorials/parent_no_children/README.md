# Tutorial 2 - Gridded Component: Create a Field and Write Out Via History
In this tutorial we will take the Hello World example a step further and demonstrate more features. I will only focus on what is added here so make sure you understand the Hello World example first.

Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/parent_with_no_children/ParentNoChildren_GridComp.F90




# ParentNoChildren_GridComp.F90

The user will notice several new things in this example. First look at the setServices routine. Notice the two MAPL_AddExportSpec calls. Each call tells the component to create an ESMF_Field in the components Export state and information about the dimensionality of the field. In this example output1 is a 2D field with no vertical levels and output2 is a 3D field. This call merely tells MAPL to create the field but does not actually create it until the components MAPL_GenericInitialize is run.

The my_initialize routine looks the same as the Hello World example.

Finally the my_run call now has some new stuff. First the user will notice some new declarations, a couple of real pointers as well as a MAPL_MetaComp object.
The MAPL_MetaComp is an internal derived type stored in the gridded component that stores MAPL specific information beyond what ESMF stores.
Past the declarations, we see we first retrieve the MAPL_MetaComp from the gridded component. Next, we call MAPL_GetResource which is a shorthand way to retrieve information from the components rc file which in this case is "root.rc". The call is looking for a key name "my_value:" and if the user examines the rc file they indeed will see this line:
```
my_value: 11.0
```
Finally there are two calls to MAPL_GetPointer which is a shorthand way to obtain a Fortran pointer to the data in an ESMF_Field, contained in an ESMF_State. Through the magic of MAPL, the user will find that there are indeed two fields in the state named ouput1 and output2! All this was handled by MAPL and ESMF!. Notice that a check is mde to determine if the pointer is associated before using it. Only if the pointer is actually associated can it be used. If it is associated, in this case all the values of the array are set to the constant my_constat. Why do we check the associated status? Because exports might not have been allocated. Imports always are, so the rule is for any pointer from an Export state, always check the associated status before using it.

$ HISTORY.rc

If one looks in the tutorial directory for this example you will see the History.rc contains these lines:
```
GRID_LABELS:
::

COLLECTIONS: my_collection
::

my_collection.template: "%y4%m2%d2_%h2%n2z.nc4"
my_collection.format: 'CFIO'
my_collection.frequency: 060000
my_collection.fields: 'output1', 'root'
   ::
```
The HISTORY.rc drives the MAPL_HistoryGridComp which is a special service provided by MAPL to allow users to write fields from any component's export state to a file. Documentation for the input file can be found [here](https://github.com/GEOS-ESM/MAPL/wiki/MAPL-History-Component).  In this example we are saying every 6 hours write the field 'output1' from the component root. You should see that in your run directory you have 4 files named starting with my_collection. If you ncdump them you will see that the variable output1 should be identically 11 in each one. If you examine the output from the run, you will see message when History writes a file, for example:
```
 AGCM Date: 2007/08/01  Time: 01:00:00  Throughput(days/day)[Avg Tot Run]:     998447.8    1017616.9   22162362.2  TimeRemaining(Est) 000:00:00    2.8% :  13.5% Mem Comm:Used
 AGCM Date: 2007/08/01  Time: 02:00:00  Throughput(days/day)[Avg Tot Run]:   24850021.6   12648460.0   51528614.8  TimeRemaining(Est) 000:00:00    2.8% :  13.5% Mem Comm:Used
 AGCM Date: 2007/08/01  Time: 03:00:00  Throughput(days/day)[Avg Tot Run]:   16222750.9   14134794.7   55756268.3  TimeRemaining(Est) 000:00:00    2.8% :  13.5% Mem Comm:Used
 AGCM Date: 2007/08/01  Time: 04:00:00  Throughput(days/day)[Avg Tot Run]:   13864970.4   13973735.3   49224105.6  TimeRemaining(Est) 000:00:00    2.8% :  13.5% Mem Comm:Used
 AGCM Date: 2007/08/01  Time: 05:00:00  Throughput(days/day)[Avg Tot Run]:   12915278.6   14773101.2   58278111.3  TimeRemaining(Est) 000:00:00    2.8% :  13.5% Mem Comm:Used

 Writing:      1 Slices to File:  my_collection.20070801_0600z.nc4
```


# Exercise for the User

The user may want to print the size of the ptr_2d and ptr_3d array to confirm that they match the size of the grid.
The user may also notice that in the files only the output1 field was written. Try adding output2 to the HISTORY.rc and see what happens.

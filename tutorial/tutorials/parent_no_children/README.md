# Tutorial 2 - Gridded Component: Create a Field and Write Out Via History
In this tutorial we will take the Hello World example a step further and demonstrate more features. I will only focus on what is added here so make sure you understand the Hello World example first.

Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/parent_with_no_children/ParentNoChildren_GridComp.F90




# ParentNoChildren_GridComp.F90

The user will notice several new things in this example. First look at the setServices routine. Notice the two MAPL_AddExportSpec calls. The each call tells the component to create an ESMF_Field in the components Export state and information about the dimensionality of the field. In this example output1 is a 2D field with no vertical levels and output2 is a 3D field. This call merely tells MAPL to create the field but does not actually create it until the components MAPL_GenericInitialize is run.

The my_initalize routine looks the same as the Hello World example.

Finally the my_run call now has some new stuff. First the user will notice some new declarations, a couple of real pointers as well as a MAPL_MetaComp object.
The MAPL_MetaComp is an internal dervied type stored in the gridded component that stores MAPL specific information beyeond what ESMF stores.
Move past the declarations we see first we retrieve the MAPL_MetaComp from the gridded component. Next we call MAPL_GetResource which is a shorthand way to retreive information from the components rc file which in this case is "root.rc". The call is looking for a key name "my_value:" and if the user examines the rc file they indeed will see this line:
```
my_value: 11.0
```
Finally there are two call so MAPL_GetPointer which is a shorthand way to obtain a the pointer to the data in an ESMF_Field in an ESMF_State. Through the magic of MAPL, the user will find that there are indeed two fields in the state named ouput1 and output2! All this was handled by MAPL and ESMF!. Notice we check if the pointer is associated before suing and if so set all the values of the pointer to the constant my_constat. Why do we check the associated status, because exports might not have been allocated. Imports always are so the rule is for any pointer from an Export state, always check the associated status before using.

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
The HISTORY.rc drives the MAPL_HistoryGridComp which is a special service provided by MAPL to allow users to write fields from any components export state to a file. Documentation for the input file can be found here: [History Documentation](https://github.com/GEOS-ESM/MAPL/wiki/MAPL-History-Component).  In this example we are saying every 6 hours write the field 'output1' from the component root. You should see that in your run directory you have 4 files named starting with my collection. If you ncdump them you will see that the variable output1 should be identically 11 in each one. Indeed you should see while that the program runs it will write a message when History writes a file.


# Exercise for the User

The user may want to print the size of the ptr_2d and ptr_3d array to confirm that they match the size of the grid.
Notice that HISTORY.rc is only outputting output1, add output2.

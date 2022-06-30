# Tutorial 3 - Simple Hierarchy
In this tutorial we take things a step further and now create a MAPL hierarchy. Please be sure you understand everything in the previous tutorialbefore moving on to this one.

Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/leaf_comp_a/AAA_GridComp.F90
tutorial/grid_comps/parent_with_one_child/ParentOneChild_GridComp.F90

# ParentOneChild_GridComp.F90

In this tutorial this is the "root" gridded component. Let's go over what's new. First notice that the setservices has a MAPL_AddChild call. We are telling it that we will add a child in the MAPL hierarchy. In this example the name of the child is obtained from the rc file which is again "root.rc" and we tell it the name of the library that will contain the code for the gridded component, also from the rc file.

Note that other than a few places in the full GEOSgcm model you will see MAPL_AddChild being done slightly differently (the exception is the Ocean gridded components). Usually we explicit "use" a module and pass a pointer to the setservices to MAPL_AddChild but this requires knowing what module you will use at compile time. For the tutorial this is not desirable. Do not get hung up on this. 

Finally in the my_initialize and my_run there are no new wrinkles other than that my_run now calls MAPL_GenericRunChildren. If this call is not made the run method of any children, grandchildren etc will not be executed.

# AAA_GridComp.F90

Now we have our first child component. It should look very familiar. It registers an initialize and run as well as adding an export spec.

One important point is that its my-initialize does not call MAPL_GridCreate. This is because the component will use the same grid as its parent. In fact you could delete my_initialize and the SetEntryPoint call in this module since if no user initialize is registered, MAPL_GenericInitialize is called automatically! Try it and see.

Now we get to the run method. Most of this should look the same but now it is adding something slightly more interesting filling the export field with time varying data. In this case I get the start time and current time from the clock and get the difference between the two in hours. I set the field to this value.

# HISTORY.rc

Now notice the HISTORY.rc has an extra line in the "fields" definition.
```
my_collection.fields: 'output1', 'root' , 'root_output_field'
                      'field1', 'AAA',
   ::
```
Here it says write out field1 from the component "AAA". The AAA component was added with the name "AAA". If you examine these output files you should notice that output1 is constant where as field1 varies in time.

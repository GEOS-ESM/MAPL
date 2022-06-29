# Tutorial 4- Simple Hierarchy with Siblings
In this tutorial we take things a step further and now create a MAPL hierachy of a parent with two Siblings. Please besure you understand everything in the previous before moving on to this one.

Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/leaf_comp_a/AAA_GridComp.F90
tutorial/grid_comps/leaf_comp_b/BBB_GridComp.F90
tutorial/grid_comps/parent_with_two_children/ParentTwoSiblings_GridComp.F90

# ParentTwoSiblings_GridComp.F90

In this tutorial this is "root" gridded component. Lets go over what's new. First notice that the serservices has two MAPL_AddChild calls now. Also notice the MAPL_AddConnectivity call. This says that an a field named "field1" from the export state of child1 (AAA) will be "connected" to a field named "field1" in the import state of child2 (BBB). In pratice what happens behind the scenes is that field1 the respective states actually a pointer to the same memory, so anytime AAA touches field1 in it's export state this is reflected in field1 in the import state of BBB.

# AAA_GridComp.F90

This is the same as the previous tutorial so nothing more needs said

# BBB_GridComp.F90
This looks similiar to the AAA gridcomp but now it does a MAPL_AddImportSpec call instead of a MAPL_AddExport Spec call. This adds a field named field1 to it's import state. In the run method we get a pointer to field1 and write the maximum value. Since this is an import field we do not need to protect the pointer with an if (associated) check.

# Running
When you run this exmaple you should notice the print from BBB each timestep and this should be increasing by 1. That's because it is "connected" to the export from AAA in this example.

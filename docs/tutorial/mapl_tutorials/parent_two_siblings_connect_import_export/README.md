# Tutorial 4- Simple Hierarchy with Siblings
In this tutorial we take things a step further and now create a MAPL hierarchy of a parent with two Siblings. Please be sure you understand everything in the previous before moving on to this one.

Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/leaf_comp_a/AAA_GridComp.F90
tutorial/grid_comps/leaf_comp_b/BBB_GridComp.F90
tutorial/grid_comps/parent_with_two_children/ParentTwoSiblings_GridComp.F90

# ParentTwoSiblings_GridComp.F90

In this tutorial this is "root" gridded component. Lets go over what's new. First notice that the setservices has two MAPL_AddChild calls now and adds two child components defined in `AAA_GridComp.F90` and `BBB_GridComp.F90`. Also notice the MAPL_AddConnectivity call. This says that a field named "field1" from the export state of child1 (AAA) will be "connected" to a field named "field1" in the import state of child2 (BBB). In practice what happens behind the scenes is that field1 the respective states actually a pointer to the same memory, so any time AAA touches field1 in its export state this is reflected in field1 in the import state of BBB.

# AAA_GridComp.F90

This is the same as the previous tutorial so nothing more needs to be said.

# BBB_GridComp.F90
This looks similar to the AAA gridcomp but now it does a MAPL_AddImportSpec call instead of a MAPL_AddExport Spec call. This adds a field named field1 to its import state. In the run method we get a pointer to field1 and write the maximum value. Since this is an import field we do not need to protect the pointer with an if (associated) check.

# How Imports are Handled in a MAPL Hierachy
This section will discuss how imports are handled in a MAPL hierarchy. As stated BBB creates a field in the import state. In fitting with the ESMF symantics and conventions the component should not touch or modify the contents of the import state so something else will nee to fill it with data. The general rule is that a parent "inherits" all the imports of its children. In practice what this means is that if the child has an field in its import state named foo, the parent will also get a field in its import state named foo. Moreover both fields will point to the same underlying pointer so are literally referencing the same memory. If the parent contains a` MAPL_AddConnectivity` call as in this exmaple the import field of the child is connected a field in the export state of another child. In MAPL when we say connected what is actually happening is that both the field in the import state and export state of the components shared the same pointer to the physical memory. The `MAPL_AddConnectivity` call also has another effect. It prevents the parent or grandparent etc of the referenced import field from being added to to those gridded component's import states.

What would happen in this example if we did not have the `MAPL_AddConnectivity`? That is an important question and will be discussed in a later tutorial.

# Running
When running this example you will notice the print from BBB each timestep and this should be increasing by 1. That's because it is "connected" to the export from AAA in this example.

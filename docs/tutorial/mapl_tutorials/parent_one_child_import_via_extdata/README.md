# Tutorial 5 - Simple Hierarchy with one child and using ExtData
In this tutorial we take things a step further and now create a MAPL hierarchy of a parent and one child. This time we use component BBB as the child. Please be sure you understand everything in the previous tutorial before moving on to this one.

Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/leaf_comp_b/BBB_GridComp.F90
tutorial/grid_comps/parent_with_one_child/ParentOneChild_GridComp.F90

# ParentOneChild_GridComp.F90

This is the same as the earlier tutorial and the "root" component.

# BBB_GridComp.F90
This is the same as the previous tutorial

# Running
In this example we use the same components you have seen before. But now our child has an import but there is no other child to make a connectivity with to fill it!. But you will see that the print from BBB has a value that is changing on each step. How is that possible? The answer is the other special MAPL gridded component, ExtData. During the run you will see lines like this:
```
   EXTDATA: Updating L bracket for field1
   EXTDATA:  ... file processed: extdata_input.200708.nc4
   EXTDATA: Updating R bracket for field1
   EXTDATA:  ... file processed: extdata_input.200708.nc4
```
In the tutorial with two childrun under root we discussed how import "bubble up" to their parents. In this case since there is no connectivity here here, the import bubbles up to the MAPL_Cap. At this point any imports that have reached the MAPL_Cap are handed off to a special component named ExtData. This is a special component that is delivered ESMF fields and uses "rules" from an input file to fill these fields with data from NetCDF files on the disk. It is used for time varying quantities like emissions and forcing data. If you look in your input files you will see that ExtData.rc has an entry that starts with "field1". This is a "rule" that tell it how to fill a variable named "field1" from a datafile. More imformation about ExtData [here](https://github.com/GEOS-ESM/MAPL/wiki/MAPL-ExtData-Component).

Also note that ExtData is currently undergoing a transition to use a new input format which will use YAML rather than the `ESMF_Config` format. Information about that format can be found [here](https://github.com/GEOS-ESM/MAPL/wiki/ExtData-Next-Generation---User-Guide).


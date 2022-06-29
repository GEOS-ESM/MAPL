# Tutorial 5 - Simple Hierarchy with one child and uisng ExtData
In this tutorial we take things a step further and now create a MAPL hierachy of a parent and one child. This time we use component BBB as the child. Please besure you understand everything in the previous before moving on to this one.

Note the code for the gridded component used by this tutorial can be found here:

tutorial/grid_comps/leaf_comp_b/BBB_GridComp.F90
tutorial/grid_comps/parent_with_one_child/ParentOneChild_GridComp.F90

# ParentOneChild_GridComp.F90

This is the same as the earlier tutorial and the "root" component.

# BBB_GridComp.F90
This is the same as the previous totorial

# Running
In this example we use the same components you have seen before. But now our child has an import and nobody fills it!. But you see that the print from BBB has a value, how is that possible? The answer is the other special MAPL gridded component, ExtData. In during the run you will see lines like this:
```
   EXTDATA: Updating L bracket for field1
   EXTDATA:  ... file processed: extdata_input.200708.nc4
   EXTDATA: Updating R bracket for field1
   EXTDATA:  ... file processed: extdata_input.200708.nc4
```
What is going on is that since there was no connectivity in the "root" component the Import is field filled by a data file on the disk from the "ExtData" component. This is a special component that is used to fill fields from the disk. It is used for time varying quantities like emissions and forcing data. If you look in your input files you will see that ExtData.rc has an entry that starts with "field1". This is a "rule" that tell it how to fill a variable named "field1" from a datafile. For more information about ExtData see here: [ExtData](https://github.com/GEOS-ESM/MAPL/wiki/MAPL-ExtData-Component)

